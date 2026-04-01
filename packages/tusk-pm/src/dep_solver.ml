open Std

type mode =
  | Refresh
  | Unlock

type event_sink = Tusk_model.Event.kind -> unit

let no_emit : event_sink = fun _ -> ()

let duration_ms_since = fun started ->
  Time.Instant.duration_since ~earlier:started (Time.Instant.now ()) |> Time.Duration.to_millis

type resolved_dependency = {
  dependency: Tusk_model.Lockfile.dependency;
  packages: Tusk_model.Lockfile.package list;
}

type resolution_state = {
  resolving: (string * Tusk_model.Lockfile.package_id) list;
  resolved: (string * Tusk_model.Lockfile.package) list;
}

let package_id_of_local_package = fun (pkg: Tusk_model.Package.t) ->
  Tusk_model.Lockfile.{ registry = None; name = pkg.name; version = None }

let manifest_path_for_package = fun (pkg: Tusk_model.Package.t) ->
  Path.(pkg.path / Path.v "tusk.toml")

let resolve_dependency_root = fun ~declared_from dep_path ->
  if Path.is_absolute dep_path then
    Path.normalize dep_path
  else
    Path.normalize Path.(declared_from / dep_path)

let load_manifest_toml = fun ~manifest_path ->
  match Fs.read_to_string manifest_path with
  | Error err ->
      Error ("failed to read manifest '"
      ^ Path.to_string manifest_path
      ^ "': "
      ^ IO.error_message err)
  | Ok source -> (
      match Data.Toml.parse source with
      | Ok toml -> Ok toml
      | Error err ->
          Error ("failed to parse manifest '"
          ^ Path.to_string manifest_path
          ^ "': "
          ^ Data.Toml.error_to_string err)
    )

let load_path_dependency_package = fun ~declared_from ~dependency_name dep_path ->
  let package_root = resolve_dependency_root ~declared_from dep_path in
  let manifest_path = Path.(package_root / Path.v "tusk.toml") in
  match load_manifest_toml ~manifest_path with
  | Error err ->
      Error ("failed to load path dependency '"
      ^ dependency_name
      ^ "' from "
      ^ Path.to_string dep_path
      ^ ": "
      ^ err)
  | Ok toml ->
      Tusk_model.Package.from_toml
        toml
        ~workspace_deps:[]
        ~workspace_dev_deps:[]
        ~workspace_build_deps:[]
        ~path:package_root
        ~relative_path:dep_path
      |> Result.map_error (fun err ->
        "failed to decode path dependency '"
        ^ dependency_name
        ^ "' from "
        ^ Path.to_string manifest_path
        ^ ": "
        ^ err)

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

let registry_resolution_key = fun ~registry_name ~package_name ->
  registry_name ^ ":" ^ Pkgs_ml.Sparse_index.normalized_name package_name

let empty_resolution_state = {
  resolving = [];
  resolved = [];
}

let find_resolving_package_id = fun ~(state: resolution_state) ~key ->
  List.assoc_opt key state.resolving

let find_resolved_package = fun ~(state: resolution_state) ~key ->
  List.assoc_opt key state.resolved

let add_resolving = fun ~(state: resolution_state) ~key ~package_id ->
  {
    state with resolving = (key, package_id) :: state.resolving;
  }

let remove_resolving = fun ~(state: resolution_state) ~key ->
  {
    state with resolving = List.filter (fun ((candidate, _)) -> not (String.equal candidate key)) state.resolving;
  }

let add_resolved = fun ~(state: resolution_state) ~key ~(pkg: Tusk_model.Lockfile.package) ->
  let state = remove_resolving ~state ~key in
  {
    state with resolved = (key, pkg) :: state.resolved;
  }

let materialized_root_for_registry_package = fun ~registry ~package_name ~version ->
  Pkgs_ml.Registry_cache.package_src_dir (Pkgs_ml.Registry.cache registry) ~package_name ~version

let manifest_path_for_materialized_root = fun root ->
  Path.(root / Path.v "tusk.toml")

let find_existing_external_package = fun ~registry_name ~existing_lock ~package_name ->
  match existing_lock with
  | None -> None
  | Some (lockfile: Tusk_model.Lockfile.t) ->
      List.find_opt
        (fun (pkg: Tusk_model.Lockfile.package) ->
          pkg.id.registry = Some registry_name
          && String.equal pkg.id.name package_name)
        lockfile.packages

let latest_release_of_document = fun (document: Pkgs_ml.Sparse_index.package_document) ->
  match
    List.find_opt
      (fun (release: Pkgs_ml.Sparse_index.release) ->
        String.equal release.version document.latest)
      document.releases
  with
  | Some release -> Ok release
  | None ->
      Error ("registry package '"
      ^ document.name
      ^ "' declares latest version '"
      ^ document.latest
      ^ "' but that release is missing from the sparse index document")

let lock_dependency_of_local_dependency = fun (dep: Tusk_model.Package.dependency) ->
  Tusk_model.Lockfile.{
    name = dep.name;
    package = { registry = None; name = dep.name; version = None }
  }

let merge_lock_packages = fun packages ->
  let rec loop seen acc = function
    | [] -> List.rev acc
    | (pkg: Tusk_model.Lockfile.package) :: rest ->
        let key = package_id_key pkg.id in
        if List.mem key seen then
          loop seen acc rest
        else
          loop (key :: seen) (pkg :: acc) rest
  in
  loop [] [] packages

let dependency_counts = fun (packages: Tusk_model.Lockfile.package list) ->
  List.fold_left
    (fun (runtime, build, dev) (pkg: Tusk_model.Lockfile.package) ->
      ( runtime + List.length pkg.dependencies,
        build + List.length pkg.build_dependencies,
        dev + List.length pkg.dev_dependencies ))
    (0, 0, 0)
    packages

let rec lock_package_of_local_package = fun
  ~emit
  ~mode
  ~registry
  ~existing_lock
  ~state
  ~provenance
  (pkg: Tusk_model.Package.t)
  ->
  match
    resolve_manifest_dependencies
      ~emit
      ~mode
      ~registry
      ~existing_lock
      ~state
      ~declared_from:pkg.path
      []
      []
      pkg.dependencies
  with
  | Error _ as err -> err
  | Ok (dependencies, dependency_packages, state) -> (
      match
        resolve_manifest_dependencies
          ~emit
          ~mode
          ~registry
          ~existing_lock
          ~state
          ~declared_from:pkg.path
          []
          []
          pkg.build_dependencies
      with
      | Error _ as err -> err
      | Ok (build_dependencies, build_packages, state) -> (
          match
            resolve_manifest_dependencies
              ~emit
              ~mode
              ~registry
              ~existing_lock
              ~state
              ~declared_from:pkg.path
              []
              []
              pkg.dev_dependencies
          with
          | Error _ as err -> err
          | Ok (dev_dependencies, dev_packages, state) ->
              Ok
                ( Tusk_model.Lockfile.{
                    id = package_id_of_local_package pkg;
                    path = pkg.path;
                    manifest_path = manifest_path_for_package pkg;
                    provenance;
                    dependencies;
                    build_dependencies;
                    dev_dependencies;
                  },
                  dependency_packages @ build_packages @ dev_packages,
                  state )
        )
    )

and resolve_registry_dependency = fun ~emit ~mode ~registry ~existing_lock ~state package_name ->
  let registry_name = Pkgs_ml.Registry.name registry in
  match mode, find_existing_external_package ~registry_name ~existing_lock ~package_name with
  | Refresh, Some (existing_pkg: Tusk_model.Lockfile.package) ->
      Ok ({
        dependency = Tusk_model.Lockfile.{ name = package_name; package = existing_pkg.id };
        packages = [];
      }, state)
  | _ ->
      let key = registry_resolution_key ~registry_name ~package_name in
      match find_resolved_package ~state ~key with
      | Some lock_package ->
          Ok ({
            dependency = Tusk_model.Lockfile.{ name = package_name; package = lock_package.id };
            packages = [];
          }, state)
      | None -> (
          match find_resolving_package_id ~state ~key with
          | Some package_id ->
              Ok ({
                dependency = Tusk_model.Lockfile.{ name = package_name; package = package_id };
                packages = [];
              }, state)
          | None -> (
              let metadata_started = Time.Instant.now () in
              emit (Tusk_model.Event.PackageMetadataFetchStarted { package = package_name });
              match Pkgs_ml.Registry.read_package_document registry ~package_name with
              | Error err ->
                  emit (Tusk_model.Event.PackageMetadataFetchFailed { package = package_name; error = err });
                  Error ("failed to read package document for '" ^ package_name ^ "': " ^ err)
              | Ok None ->
                  emit
                    (Tusk_model.Event.PackageMetadataFetchFailed {
                      package = package_name;
                      error = "package not found in registry"
                    });
                  Error ("package '" ^ package_name ^ "' was not found in registry '" ^ registry_name ^ "'")
              | Ok (Some document) -> (
                  emit
                    (Tusk_model.Event.PackageMetadataFetchFinished {
                      package = document.name;
                      version = Some document.latest;
                      duration_ms = duration_ms_since metadata_started
                    });
                  match latest_release_of_document document with
                  | Error _ as err -> err
                  | Ok (release: Pkgs_ml.Sparse_index.release) ->
                      let package_id =
                        Tusk_model.Lockfile.{
                          registry = Some registry_name;
                          name = document.name;
                          version = Some release.version;
                        }
                      in
                      let state = add_resolving ~state ~key ~package_id in
                      let rec resolve_release_dependencies
                        ~(state: resolution_state)
                        (acc_packages: Tusk_model.Lockfile.package list)
                        (acc_dependencies: Tusk_model.Lockfile.dependency list)
                        (release_dependencies: Pkgs_ml.Sparse_index.dependency list)
                      =
                        match release_dependencies with
                        | [] -> Ok (List.rev acc_dependencies, acc_packages, state)
                        | (dep: Pkgs_ml.Sparse_index.dependency) :: rest -> (
                            match
                              resolve_registry_dependency
                                ~emit
                                ~mode
                                ~registry
                                ~existing_lock
                                ~state
                                dep.name
                            with
                            | Error _ as err -> err
                            | Ok (resolved, state) ->
                                resolve_release_dependencies
                                  ~state
                                  (List.rev_append resolved.packages acc_packages)
                                  (resolved.dependency :: acc_dependencies)
                                  rest
                          )
                      in
                      match resolve_release_dependencies ~state [] [] release.dependencies with
                      | Error _ as err -> err
                      | Ok (dependencies, dependency_packages, state) ->
                          let path =
                            materialized_root_for_registry_package
                              ~registry
                              ~package_name:document.name
                              ~version:release.version
                          in
                          let lock_package =
                            Tusk_model.Lockfile.{
                              id = package_id;
                              path;
                              manifest_path = manifest_path_for_materialized_root path;
                              provenance = Registry { registry = registry_name };
                              dependencies;
                              build_dependencies = [];
                              dev_dependencies = [];
                            }
                          in
                          let state = add_resolved ~state ~key ~pkg:lock_package in
                          Ok ({
                            dependency = Tusk_model.Lockfile.{ name = package_name; package = lock_package.id };
                            packages = dependency_packages @ [ lock_package ];
                          }, state)
                )
            )
        )

and resolve_path_dependency = fun
  ~emit
  ~mode
  ~registry
  ~existing_lock
  ~state
  ~declared_from
  dependency_name
  dep_path
  ->
  match load_path_dependency_package ~declared_from ~dependency_name dep_path with
  | Error _ as err -> err
  | Ok pkg -> (
      match
        lock_package_of_local_package
          ~emit
          ~mode
          ~registry
          ~existing_lock
          ~state
          ~provenance:(Tusk_model.Lockfile.Path dep_path)
          pkg
      with
      | Error _ as err -> err
      | Ok (lock_package, dependency_packages, state) ->
          Ok ({
            dependency = Tusk_model.Lockfile.{ name = dependency_name; package = lock_package.id };
            packages = dependency_packages @ [ lock_package ];
          }, state)
    )

and resolve_manifest_dependencies = fun
  ~emit
  ~mode
  ~registry
  ~existing_lock
  ~state
  ~declared_from
  acc_packages
  acc_dependencies
  deps
  ->
  match deps with
  | [] -> Ok (List.rev acc_dependencies, List.rev acc_packages, state)
  | dep :: rest -> (
      match dep.Tusk_model.Package.source with
      | Tusk_model.Package.Workspace ->
          resolve_manifest_dependencies
            ~emit
            ~mode
            ~registry
            ~existing_lock
            ~state
            ~declared_from
            acc_packages
            (lock_dependency_of_local_dependency dep :: acc_dependencies)
            rest
      | Tusk_model.Package.Registry _ -> (
          match
            resolve_registry_dependency
              ~emit
              ~mode
              ~registry
              ~existing_lock
              ~state
              dep.name
          with
          | Error _ as err -> err
          | Ok (resolved, state) ->
            resolve_manifest_dependencies
                ~emit
                ~mode
                ~registry
                ~existing_lock
                ~state
                ~declared_from
                (List.rev_append resolved.packages acc_packages)
                (resolved.dependency :: acc_dependencies)
                rest
        )
      | Tusk_model.Package.Path path -> (
          match
            resolve_path_dependency
              ~emit
              ~mode
              ~registry
              ~existing_lock
              ~state
              ~declared_from
              dep.name
              path
          with
          | Error _ as err -> err
          | Ok (resolved, state) ->
              resolve_manifest_dependencies
                ~emit
                ~mode
                ~registry
                ~existing_lock
                ~state
                ~declared_from
                (List.rev_append resolved.packages acc_packages)
                (resolved.dependency :: acc_dependencies)
                rest
        )
    )

let lock_package_of_workspace_package = fun ~emit ~mode ~registry ~existing_lock ~state (pkg: Tusk_model.Package.t) ->
  lock_package_of_local_package
    ~emit
    ~mode
    ~registry
    ~existing_lock
    ~state
    ~provenance:Tusk_model.Lockfile.Workspace
    pkg

let rec lock_packages = fun ~emit ~mode ~registry ~existing_lock ~state acc_workspace acc_external packages ->
  match packages with
  | [] -> Ok (List.rev acc_workspace, List.rev acc_external, state)
  | pkg :: rest -> (
      match
        lock_package_of_workspace_package
          ~emit
          ~mode
          ~registry
          ~existing_lock
          ~state
          pkg
      with
      | Ok (pkg, external_packages, state) ->
          lock_packages
            ~emit
            ~mode
            ~registry
            ~existing_lock
            ~state
            (pkg :: acc_workspace)
            (List.rev_append external_packages acc_external)
            rest
      | Error _ as err -> err
    )

let keep_existing_package = fun workspace_packages (pkg: Tusk_model.Lockfile.package) ->
  let workspace_names =
    List.map (fun (pkg: Tusk_model.Package.t) -> pkg.name) workspace_packages
  in
  not (List.mem pkg.id.name workspace_names)

let lock_deps = fun ?(emit = no_emit) ~mode ~registry ~existing_lock packages ->
  let started = Time.Instant.now () in
  emit
    (Tusk_model.Event.DependencyUniverseBuilding {
      packages = List.map (fun (pkg: Tusk_model.Package.t) -> pkg.name) packages
    });
  match
    lock_packages
      ~emit
      ~mode
      ~registry
      ~existing_lock
      ~state:empty_resolution_state
      []
      []
      packages
  with
  | Ok (workspace_packages, external_packages, _state) ->
      let preserved =
        match (mode, existing_lock) with
        | Unlock, _ -> []
        | Refresh, Some (existing_lock: Tusk_model.Lockfile.t) ->
            List.filter (keep_existing_package packages) existing_lock.packages
        | Refresh, None -> []
      in
      let packages = merge_lock_packages (workspace_packages @ external_packages @ preserved) in
      let runtime_packages, build_packages, dev_packages = dependency_counts packages in
      emit
        (Tusk_model.Event.DependencyUniverseBuilt {
          runtime_packages;
          build_packages;
          dev_packages;
          duration_ms = duration_ms_since started
        });
      Ok Tusk_model.Lockfile.{ format_version = 1; packages }
  | Error _ as err -> err
