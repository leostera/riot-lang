open Std
open Std.Result.Syntax

module Error = Error

type event_sink = Riot_model.Event.deps_event -> unit

let no_emit: event_sink = fun _ -> ()

let duration_ms_since = fun started ->
  Time.Instant.duration_since ~earlier:started (Time.Instant.now ())
  |> Time.Duration.to_millis

type package_materialization_state =
  | Already_materialized
  | Needs_materialization_from_cache
  | Needs_download

let materialization_state = fun ~registry ~(pkg:Riot_model.Lockfile.package) ->
  match pkg.id.version with
  | None -> Ok Already_materialized
  | Some version ->
      let package_name = Riot_model.Package_name.to_string pkg.id.name in
      let manifest_path =
        Pkgs_ml.Registry_cache.package_src_dir
          (Pkgs_ml.Registry.cache registry)
          ~package_name
          ~version
        |> fun root -> Path.(root / Path.v "riot.toml")
      in
      match Fs.exists manifest_path with
      | Error err ->
          Error (Error.MaterializationFailed {
            error = "failed to check package manifest '"
            ^ Path.to_string manifest_path
            ^ "': "
            ^ IO.error_message err;
          })
      | Ok true -> Ok Already_materialized
      | Ok false ->
          let package_name = Riot_model.Package_name.to_string pkg.id.name in
          let archive_path =
            Pkgs_ml.Registry_cache.archive_path
              (Pkgs_ml.Registry.cache registry)
              ~package_name
              ~version
          in
          match Fs.exists archive_path with
          | Error err ->
              Error (Error.MaterializationFailed {
                error = "failed to check cached package archive '"
                ^ Path.to_string archive_path
                ^ "': "
                ^ IO.error_message err;
              })
          | Ok has_archive ->
              Ok (
                if has_archive then
                  Needs_materialization_from_cache
                else
                  Needs_download
              )

let ensure_registry_package = fun
  ?(emit = no_emit) ~registry ~(pkg:Riot_model.Lockfile.package) () ->
  let event_package = pkg.id.name in
  let package = Riot_model.Package_name.to_string event_package in
  match pkg.id.version with
  | None ->
      Error (Error.MaterializationFailed {
        error = "registry lock package '" ^ package ^ "' is missing an exact version";
      })
  | Some version -> (
      let root =
        Pkgs_ml.Registry_cache.package_src_dir
          (Pkgs_ml.Registry.cache registry)
          ~package_name:package
          ~version
      in
      let path = Path.to_string root in
      let* state = materialization_state ~registry ~pkg in
      match state with
      | Already_materialized ->
          emit
            (
              Riot_model.Event.DepsPackageDownloadSkipped {
                package = event_package;
                version;
                path;
                reason = "package source tree already exists in the registry cache";
              }
            );
          Ok root
      | Needs_materialization_from_cache
      | Needs_download ->
          let started = Time.Instant.now () in
          emit
            (Riot_model.Event.DepsPackageMaterializationStarted {
              package = event_package;
              version;
              path;
            });
          if state = Needs_download then
            emit
              (Riot_model.Event.DepsPackageDownloadStarted {
                package = event_package;
                version;
                path;
              });
          match Pkgs_ml.Registry.materialize_release registry ~package_name:package ~version with
          | Ok Pkgs_ml.Registry.Materialized ->
              emit
                (
                  Riot_model.Event.DepsPackageMaterializationFinished {
                    package = event_package;
                    version;
                    path;
                    duration_ms = duration_ms_since started;
                  }
                );
              Ok root
          | Ok Pkgs_ml.Registry.Already_present ->
              emit
                (
                  Riot_model.Event.DepsPackageDownloadSkipped {
                    package = event_package;
                    version;
                    path;
                    reason = "package source tree already exists in the registry cache";
                  }
                );
              Ok root
          | Error err ->
              let error = Error.MaterializationFailed { error = err } in
              emit
                (
                  Riot_model.Event.DepsPackageMaterializationFailed {
                    package = event_package;
                    version;
                    path;
                    error;
                  }
                );
              Error error
    )

let ensure_packages = fun ?(emit = no_emit) ~registry ~(lockfile:Riot_model.Lockfile.t) () ->
  let rec loop = fun __tmp1 ->
    match __tmp1 with
    | [] -> Ok ()
    | (pkg: Riot_model.Lockfile.package) :: rest -> (
        match pkg.provenance with
        | Riot_model.Lockfile.Workspace
        | Riot_model.Lockfile.Path _
        | Riot_model.Lockfile.Source _ -> loop rest
        | Riot_model.Lockfile.Registry _ -> (
            match ensure_registry_package ~emit ~registry ~pkg () with
            | Ok _ -> loop rest
            | Error _ as err -> err
          )
      )
  in
  loop lockfile.packages
