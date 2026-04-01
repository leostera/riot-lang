open Std
module Error = Error

type event_sink = Tusk_model.Event.kind -> unit

let no_emit : event_sink = fun _ -> ()

let duration_ms_since = fun started ->
  Time.Instant.duration_since ~earlier:started (Time.Instant.now ()) |> Time.Duration.to_millis

let package_id_key = fun (id: Tusk_model.Lockfile.package_id) ->
  let registry =
    match id.registry with
    | Some registry -> registry
    | None -> "workspace"
  in
  let version =
    match id.version with
    | Some version -> version
    | None -> "workspace"
  in
  registry ^ ":" ^ id.name ^ ":" ^ version

let workspace_package_id_of_package = fun (package: Tusk_model.Package.t) ->
  Tusk_model.Lockfile.{ registry = None; name = package.name; version = None }

let find_lock_package_by_id = fun ~(package_id:Tusk_model.Lockfile.package_id) ~(lockfile:Tusk_model.Lockfile.t) ->
  List.find_opt
    (fun (lock_package: Tusk_model.Lockfile.package) -> lock_package.id = package_id)
    lockfile.packages

let find_workspace_package_by_id = fun ~(package_id:Tusk_model.Lockfile.package_id) ~(packages:Tusk_model.Package.t list) ->
  List.find_opt
    (fun (package: Tusk_model.Package.t) -> workspace_package_id_of_package package = package_id)
    packages

let materialized_root_of_lock_package = fun ~registry ~workspace_root ~(lock_package:Tusk_model.Lockfile.package) ->
  match lock_package.provenance with
  | Tusk_model.Lockfile.Workspace -> (
      match lock_package.root with
      | Some root -> Ok Path.(workspace_root / root)
      | None ->
          Error (Error.ProjectionFailed {
            error = "workspace lock package '" ^ lock_package.id.name ^ "' is missing a portable root"
          })
    )
  | Tusk_model.Lockfile.Path _ -> (
      match lock_package.root with
      | Some root when Path.is_absolute root -> Ok root
      | Some root -> Ok Path.(workspace_root / root)
      | None ->
          Error (Error.ProjectionFailed {
            error = "path lock package '" ^ lock_package.id.name ^ "' is missing a portable root"
          })
    )
  | Tusk_model.Lockfile.Registry { registry=registry_name } -> (
      match lock_package.id.version with
      | None ->
          Error (Error.ProjectionFailed {
            error = "registry lock package '" ^ lock_package.id.name ^ "' is missing an exact version"
          })
      | Some version ->
          if not (String.equal registry_name (Pkgs_ml.Registry.name registry)) then
            Error (Error.ProjectionFailed {
              error =
                "lockfile references registry '"
                ^ registry_name
                ^ "' but active registry is '"
                ^ Pkgs_ml.Registry.name registry
                ^ "'"
            })
          else
            Ok (Pkgs_ml.Registry_cache.package_src_dir
              (Pkgs_ml.Registry.cache registry)
              ~package_name:lock_package.id.name
              ~version)
    )

let manifest_path_of_root = fun root -> Path.(root / Path.v "tusk.toml")

let load_manifest_toml = fun ~manifest_path ->
  match Fs.read manifest_path with
  | Error err ->
      Error (Error.ManifestReadFailed {
        manifest_path;
        error = IO.error_message err
      })
  | Ok source -> (
      match Data.Toml.parse source with
      | Ok toml -> Ok toml
      | Error err ->
          Error (Error.ManifestParseFailed {
            manifest_path;
            error = Data.Toml.error_to_string err
          })
    )

let load_external_package = fun ~emit ~registry ~workspace_root ~(lock_package:Tusk_model.Lockfile.package) ->
  let version_opt = lock_package.id.version in
  let package_name = lock_package.id.name in
  match materialized_root_of_lock_package ~registry ~workspace_root ~lock_package with
  | Error _ as err -> err
  | Ok package_root ->
      let started = Time.Instant.now () in
      let emit_started =
        match version_opt with
        | Some version -> emit
          (Tusk_model.Event.PackageManifestFetchStarted { package = package_name; version })
        | None -> ()
      in
      let emit_finished () =
        match version_opt with
        | Some version -> emit
          (Tusk_model.Event.PackageManifestFetchFinished {
            package = package_name;
            version;
            duration_ms = duration_ms_since started
          })
        | None -> ()
      in
      let emit_failed error = emit
        (Tusk_model.Event.PackageManifestFetchFailed {
          package = package_name;
          version = version_opt;
          error
        }) in
      let manifest_path = manifest_path_of_root package_root in
      emit_started;
      match load_manifest_toml ~manifest_path with
      | Error err ->
          emit_failed err;
          Error err
      | Ok toml ->
          Tusk_model.Package.from_toml toml ~workspace_deps:[] ~workspace_dev_deps:[] ~workspace_build_deps:[] ~path:package_root
            ~relative_path:((
              match lock_package.root with
              | Some root -> root
              | None -> package_root
            ))
          |> Result.map
            (fun pkg ->
              emit_finished ();
              pkg)
          |> Result.map_error
            (fun err ->
              let err = Error.ProjectionFailed { error = err } in
              emit_failed err;
              err)

let load_package_for_lock_package = fun ~emit ~registry ~workspace_root ~(packages:Tusk_model.Package.t list) ~(lock_package:Tusk_model.Lockfile.package) ->
  match lock_package.provenance with
  | Tusk_model.Lockfile.Workspace -> (
      match find_workspace_package_by_id ~package_id:lock_package.id ~packages with
      | Some package -> Ok package
      | None ->
          Error (Error.ProjectionFailed {
            error = "workspace package '" ^ lock_package.id.name ^ "' was not provided to projection"
          })
    )
  | Tusk_model.Lockfile.Path _
  | Tusk_model.Lockfile.Registry _ -> load_external_package ~emit ~registry ~workspace_root ~lock_package

let resolve_dependency_ids = fun (resolved: Tusk_model.Package.resolved) ->
  List.map (fun (dep: Tusk_model.Package.resolved_dependency) -> dep.resolved_id) resolved.runtime_resolved
  @ List.map (fun (dep: Tusk_model.Package.resolved_dependency) -> dep.resolved_id) resolved.build_resolved
  @ List.map (fun (dep: Tusk_model.Package.resolved_dependency) -> dep.resolved_id) resolved.dev_resolved

let rec resolve_package_graph = fun ~emit ~registry ~workspace_root ~(packages:Tusk_model.Package.t list) ~(lockfile:Tusk_model.Lockfile.t) seen acc pending ->
  match pending with
  | [] -> Ok (List.rev acc)
  | package_id :: rest ->
      let key = package_id_key package_id in
      if List.mem key seen then
        resolve_package_graph ~emit ~registry ~workspace_root ~packages ~lockfile seen acc rest
      else
        match find_lock_package_by_id ~package_id ~lockfile with
        | None ->
            Error (Error.ProjectionFailed {
              error = "lockfile is missing package '" ^ package_id.name ^ "'"
            })
        | Some lock_package -> (
            match load_package_for_lock_package ~emit ~registry ~workspace_root ~packages ~lock_package with
            | Error _ as err -> err
            | Ok package -> (
                let materialized_root =
                  match lock_package.provenance with
                  | Tusk_model.Lockfile.Workspace -> package.path
                  | Tusk_model.Lockfile.Path _
                  | Tusk_model.Lockfile.Registry _ -> materialized_root_of_lock_package
                    ~registry
                    ~workspace_root
                    ~lock_package
                  |> Result.expect ~msg:"expected lock package root to be derivable"
                in
                let manifest_path = manifest_path_of_root materialized_root in
                match Tusk_model.Package.resolve ~package ~lock_package ~manifest_path ~materialized_root with
                | Error err ->
                    Error (Error.ProjectionFailed { error = err })
                | Ok resolved ->
                    emit
                      (Tusk_model.Event.PackageResolvedForBuild {
                        package = resolved.id.name;
                        version = resolved.id.version;
                        path = Path.to_string resolved.materialized_root;
                        workspace = resolved.provenance = Tusk_model.Lockfile.Workspace
                      });
                    let dependency_ids = resolve_dependency_ids resolved in
                    resolve_package_graph
                      ~emit
                      ~registry
                      ~workspace_root
                      ~packages
                      ~lockfile
                      (key :: seen)
                      (resolved :: acc)
                      (dependency_ids @ rest)
              )
          )

let resolve_packages = fun ?(emit = no_emit) ~registry ~workspace_root ~packages ~lockfile () ->
  let root_ids = List.map workspace_package_id_of_package packages in
  resolve_package_graph ~emit ~registry ~workspace_root ~packages ~lockfile [] [] root_ids
