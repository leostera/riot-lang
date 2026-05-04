open Std
open Std.Result.Syntax

module Error = Error

type event_sink = Riot_model.Event.kind -> unit

let no_emit: event_sink = fun _ -> ()

let duration_ms_since = fun started ->
  Time.Instant.duration_since ~earlier:started (Time.Instant.now ())
  |> Time.Duration.to_millis

let package_id_key = fun (id: Riot_model.Lockfile.package_id) ->
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
  registry ^ ":" ^ Riot_model.Package_name.to_string id.name ^ ":" ^ version

let workspace_package_id_of_package = fun (package: Riot_model.Package_manifest.t) ->
  Riot_model.Lockfile.{
    registry = None;
    name = package.name;
    version = None;
    sha256 = None;
  }

let find_lock_package_by_id = fun
  ~(package_id:Riot_model.Lockfile.package_id) ~(lockfile:Riot_model.Lockfile.t) ->
  List.find
    lockfile.packages
    ~fn:(fun (lock_package: Riot_model.Lockfile.package) -> lock_package.id = package_id)

let find_workspace_package_by_id = fun
  ~(package_id:Riot_model.Lockfile.package_id) ~(packages:Riot_model.Package_manifest.t list) ->
  List.find
    packages
    ~fn:(fun (package: Riot_model.Package_manifest.t) ->
      workspace_package_id_of_package package = package_id)

let materialized_root_of_lock_package = fun
  ~materialize_emit ~registry ~workspace_root ~(lock_package:Riot_model.Lockfile.package) ->
  match lock_package.provenance with
  | Riot_model.Lockfile.Workspace -> (
      match lock_package.root with
      | Some root -> Ok Path.(workspace_root / root)
      | None ->
          Error (Error.ProjectionFailed {
            error = "workspace lock package '"
            ^ Riot_model.Package_name.to_string lock_package.id.name
            ^ "' is missing a portable root";
          })
    )
  | Riot_model.Lockfile.Path _ -> (
      match lock_package.root with
      | Some root when Path.is_absolute root -> Ok root
      | Some root -> Ok Path.(workspace_root / root)
      | None ->
          Error (Error.ProjectionFailed {
            error = "path lock package '"
            ^ Riot_model.Package_name.to_string lock_package.id.name
            ^ "' is missing a portable root";
          })
    )
  | Riot_model.Lockfile.Source { locator; ref_ } -> (
      match Git_dependency.materialize ~source_locator:locator ~ref_ () with
      | Ok materialized -> Ok materialized.package_root
      | Error error -> Error (Error.ProjectionFailed { error = Git_dependency.message error })
    )
  | Riot_model.Lockfile.Registry { registry = registry_name } -> (
      match lock_package.id.version with
      | None ->
          Error (Error.ProjectionFailed {
            error = "registry lock package '"
            ^ Riot_model.Package_name.to_string lock_package.id.name
            ^ "' is missing an exact version";
          })
      | Some version ->
          if not (String.equal registry_name (Pkgs_ml.Registry.name registry)) then
            Error (Error.ProjectionFailed {
              error = "lockfile references registry '"
              ^ registry_name
              ^ "' but active registry is '"
              ^ Pkgs_ml.Registry.name registry
              ^ "'";
            })
          else
            Materializer.ensure_registry_package
              ~emit:materialize_emit
              ~registry
              ~pkg:lock_package
              ()
            |> Result.map_err
              ~fn:(fun err -> Error.ProjectionFailed { error = Error.message err })
    )

let manifest_path_of_root = fun root -> Path.(root / Path.v "riot.toml")

let load_manifest_toml = fun ~manifest_path ->
  match Fs.read manifest_path with
  | Error err -> Error (Error.ManifestReadFailed { manifest_path; error = IO.error_message err })
  | Ok source -> (
      match Data.Toml.parse source with
      | Ok toml -> Ok toml
      | Error err ->
          Error (Error.ManifestParseFailed {
            manifest_path;
            error = Data.Toml.error_to_string err;
          })
    )

let load_external_package = fun
  ~emit ~materialize_emit ~registry ~workspace_root ~(lock_package:Riot_model.Lockfile.package) ->
  let version_opt = lock_package.id.version in
  let package_name = lock_package.id.name in
  match materialized_root_of_lock_package ~materialize_emit ~registry ~workspace_root ~lock_package with
  | Error _ as err -> err
  | Ok package_root ->
      let started = Time.Instant.now () in
      let emit_started =
        match version_opt with
        | Some version ->
            emit (Riot_model.Event.PackageManifestFetchStarted { package = package_name; version })
        | None -> ()
      in
      let emit_finished () =
        match version_opt with
        | Some version ->
            emit
              (Riot_model.Event.PackageManifestFetchFinished {
                package = package_name;
                version;
                duration_ms = duration_ms_since started;
              })
        | None -> ()
      in
      let emit_failed error =
        emit
          (Riot_model.Event.PackageManifestFetchFailed {
            package = package_name;
            version = version_opt;
            error;
          })
      in
      let manifest_path = manifest_path_of_root package_root in
      emit_started;
      match load_manifest_toml ~manifest_path with
      | Error err ->
          emit_failed err;
          Error err
      | Ok toml ->
          Riot_model.Package_manifest.from_toml
            toml
            ~workspace_deps:[]
            ~workspace_dev_deps:[]
            ~workspace_build_deps:[]
            ~path:package_root
            ~relative_path:(
              match lock_package.root with
              | Some root -> root
              | None -> package_root
            )
          |> Result.map
            ~fn:(fun manifest ->
              emit_finished ();
              Riot_model.Package.from_manifest_spec manifest)
          |> Result.map_err
            ~fn:(fun err ->
              let err = Error.ProjectionFailed {
                error = Riot_model.Package_manifest.error_message err;
              }
              in
              emit_failed err;
              err)

let load_package_for_lock_package = fun
  ~emit
  ~materialize_emit
  ~registry
  ~workspace_root
  ~(packages:Riot_model.Package_manifest.t list)
  ~(lock_package:Riot_model.Lockfile.package) ->
  match lock_package.provenance with
  | Riot_model.Lockfile.Workspace -> (
      match find_workspace_package_by_id ~package_id:lock_package.id ~packages with
      | Some package -> Ok (Riot_model.Package.from_manifest_spec package)
      | None ->
          Error (Error.ProjectionFailed {
            error = "workspace package '"
            ^ Riot_model.Package_name.to_string lock_package.id.name
            ^ "' was not provided to projection";
          })
    )
  | Riot_model.Lockfile.Path _
  | Riot_model.Lockfile.Source _
  | Riot_model.Lockfile.Registry _ ->
      load_external_package ~emit ~materialize_emit ~registry ~workspace_root ~lock_package

let resolve_dependency_ids = fun (resolved: Riot_model.Package.resolved) ->
  (List.map
    resolved.runtime_resolved
    ~fn:(fun (dep: Riot_model.Package.resolved_dependency) -> dep.resolved_id)
  @ List.map
    resolved.build_resolved
    ~fn:(fun (dep: Riot_model.Package.resolved_dependency) -> dep.resolved_id))
  @ List.map
    resolved.dev_resolved
    ~fn:(fun (dep: Riot_model.Package.resolved_dependency) -> dep.resolved_id)

let rec resolve_package_graph = fun
  ~emit
  ~materialize_emit
  ~registry
  ~workspace_root
  ~(packages:Riot_model.Package_manifest.t list)
  ~(lockfile:Riot_model.Lockfile.t)
  seen
  acc
  pending ->
  match pending with
  | [] -> Ok (List.reverse acc)
  | package_id :: rest ->
      let key = package_id_key package_id in
      if List.contains seen ~value:key then
        resolve_package_graph
          ~emit
          ~materialize_emit
          ~registry
          ~workspace_root
          ~packages
          ~lockfile
          seen
          acc
          rest
      else
        match find_lock_package_by_id ~package_id ~lockfile with
        | None ->
            Error (Error.ProjectionFailed {
              error = "lockfile is missing package '"
              ^ Riot_model.Package_name.to_string package_id.name
              ^ "'";
            })
        | Some lock_package -> (
            match load_package_for_lock_package
              ~emit
              ~materialize_emit
              ~registry
              ~workspace_root
              ~packages
              ~lock_package with
            | Error _ as err -> err
            | Ok package -> (
                let materialized_root = package.path in
                let manifest_path = manifest_path_of_root materialized_root in
                match Riot_model.Package.resolve
                  ~package
                  ~lock_package
                  ~manifest_path
                  ~materialized_root with
                | Error err -> Error (Error.ProjectionFailed { error = err })
                | Ok resolved ->
                    emit
                      (
                        Riot_model.Event.PackageResolvedForBuild {
                          package = package.name;
                          version = resolved.id.version;
                          path = Path.to_string resolved.materialized_root;
                          workspace = resolved.provenance = Riot_model.Lockfile.Workspace;
                        }
                      );
                    let dependency_ids = resolve_dependency_ids resolved in
                    resolve_package_graph
                      ~emit
                      ~materialize_emit
                      ~registry
                      ~workspace_root
                      ~packages
                      ~lockfile
                      (key :: seen)
                      (resolved :: acc)
                      (dependency_ids @ rest)
              )
          )

let resolve_packages = fun
  ?(emit = no_emit)
  ?(materialize_emit = no_emit)
  ~registry
  ~workspace_root
  ~packages
  ~lockfile
  () ->
  let root_ids = List.map packages ~fn:workspace_package_id_of_package in
  resolve_package_graph
    ~emit
    ~materialize_emit
    ~registry
    ~workspace_root
    ~packages
    ~lockfile
    []
    []
    root_ids
