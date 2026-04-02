open Std
module Error = Error

let ( let* ) = Result.and_then

type event_sink = Riot_model.Event.kind -> unit

let no_emit : event_sink = fun _ -> ()

let duration_ms_since = fun started ->
  Time.Instant.duration_since ~earlier:started (Time.Instant.now ()) |> Time.Duration.to_millis

type package_materialization_state =
  | Already_materialized
  | Needs_materialization_from_cache
  | Needs_download

let materialization_state = fun ~registry ~(pkg:Riot_model.Lockfile.package) ->
  match pkg.id.version with
  | None -> Ok Already_materialized
  | Some version ->
      let manifest_path = Pkgs_ml.Registry_cache.package_src_dir
        (Pkgs_ml.Registry.cache registry)
        ~package_name:pkg.id.name
        ~version
      |> fun root -> Path.(root / Path.v "riot.toml") in
      match Fs.exists manifest_path with
      | Error err ->
          Error (Error.MaterializationFailed {
            error = "failed to check package manifest '"
            ^ Path.to_string manifest_path
            ^ "': "
            ^ IO.error_message err
          })
      | Ok true ->
          Ok Already_materialized
      | Ok false ->
          let archive_path = Pkgs_ml.Registry_cache.archive_path
            (Pkgs_ml.Registry.cache registry)
            ~package_name:pkg.id.name
            ~version in
          match Fs.exists archive_path with
          | Error err -> Error (Error.MaterializationFailed {
            error = "failed to check cached package archive '"
            ^ Path.to_string archive_path
            ^ "': "
            ^ IO.error_message err
          })
          | Ok has_archive ->
              Ok (
                if has_archive then
                  Needs_materialization_from_cache
                else
                  Needs_download
              )

let ensure_registry_package = fun ?(emit = no_emit) ~registry (pkg: Riot_model.Lockfile.package) ->
  let package = pkg.id.name in
  match pkg.id.version with
  | None -> Error (Error.MaterializationFailed {
    error = "registry lock package '" ^ package ^ "' is missing an exact version"
  })
  | Some version -> (
      let path = Pkgs_ml.Registry_cache.package_src_dir
        (Pkgs_ml.Registry.cache registry)
        ~package_name:package
        ~version
      |> Path.to_string in
      let* state = materialization_state ~registry ~pkg in
      match state with
      | Already_materialized ->
          emit
            (Riot_model.Event.PackageDownloadSkipped {
              package;
              version;
              path;
              reason = "package source tree already exists in the registry cache"
            });
          Ok ()
      | Needs_materialization_from_cache
      | Needs_download ->
          let started = Time.Instant.now () in
          emit (Riot_model.Event.PackageMaterializationStarted { package; version; path });
          if state = Needs_download then
            emit (Riot_model.Event.PackageDownloadStarted { package; version; path });
          match Pkgs_ml.Registry.materialize_release registry ~package_name:package ~version with
          | Ok `Materialized ->
              emit
                (Riot_model.Event.PackageMaterializationFinished {
                  package;
                  version;
                  path;
                  duration_ms = duration_ms_since started
                });
              Ok ()
          | Ok `Already_present ->
              emit
                (Riot_model.Event.PackageDownloadSkipped {
                  package;
                  version;
                  path;
                  reason = "package source tree already exists in the registry cache"
                });
              Ok ()
          | Error err ->
              let error = Error.MaterializationFailed { error = err } in
              emit (Riot_model.Event.PackageMaterializationFailed { package; version; path; error });
              Error error
    )

let ensure_packages = fun ?(emit = no_emit) ~registry ~(lockfile:Riot_model.Lockfile.t) () ->
  let rec loop = function
    | [] -> Ok ()
    | (pkg: Riot_model.Lockfile.package) :: rest -> (
        match pkg.provenance with
        | Riot_model.Lockfile.Workspace
        | Riot_model.Lockfile.Path _
        | Riot_model.Lockfile.Source _ -> loop rest
        | Riot_model.Lockfile.Registry _ -> (
            match ensure_registry_package ~emit ~registry pkg with
            | Ok () -> loop rest
            | Error _ as err -> err
          )
      )
  in
  loop lockfile.packages
