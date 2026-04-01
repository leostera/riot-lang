open Std

type event_sink = Tusk_model.Event.kind -> unit

let no_emit : event_sink = fun _ -> ()

let duration_ms_since = fun started ->
  Time.Instant.duration_since ~earlier:started (Time.Instant.now ()) |> Time.Duration.to_millis

let ensure_registry_package = fun ?(emit = no_emit) ~registry (pkg: Tusk_model.Lockfile.package) ->
  let package = pkg.id.name in
  match pkg.id.version with
  | None -> Error ("registry lock package '" ^ package ^ "' is missing an exact version")
  | Some version -> (
      let path = Pkgs_ml.Registry_cache.package_src_dir
        (Pkgs_ml.Registry.cache registry)
        ~package_name:package
        ~version
      |> Path.to_string in
      let started = Time.Instant.now () in
      emit (Tusk_model.Event.PackageMaterializationStarted { package; version; path });
      match Pkgs_ml.Registry.materialize_release registry ~package_name:package ~version with
      | Ok `Materialized ->
          emit
            (Tusk_model.Event.PackageMaterializationFinished {
              package;
              version;
              path;
              duration_ms = duration_ms_since started
            });
          Ok ()
      | Ok `Already_present ->
          emit
            (Tusk_model.Event.PackageDownloadSkipped {
              package;
              version;
              path;
              reason = "package source tree already exists in the registry cache"
            });
          Ok ()
      | Error err ->
          emit
            (Tusk_model.Event.PackageMaterializationFailed { package; version; path; error = err });
          Error err
    )

let ensure_packages = fun ?(emit = no_emit) ~registry ~(lockfile:Tusk_model.Lockfile.t) () ->
  let rec loop = function
    | [] -> Ok ()
    | (pkg: Tusk_model.Lockfile.package) :: rest -> (
        match pkg.provenance with
        | Tusk_model.Lockfile.Workspace
        | Tusk_model.Lockfile.Path _ -> loop rest
        | Tusk_model.Lockfile.Registry _ -> (
            match ensure_registry_package ~emit ~registry pkg with
            | Ok () -> loop rest
            | Error _ as err -> err
          )
      )
  in
  loop lockfile.packages
