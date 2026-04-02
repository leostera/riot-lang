open Std
module Error = Error

let ( let* ) = Result.and_then

type mode =
  | Refresh
  | Unlock

type event_sink = Riot_model.Event.kind -> unit

let no_emit : event_sink = fun _ -> ()

type context = {
  emit: event_sink;
  mode: mode;
  registry: Pkgs_ml.Registry.t;
  existing_lock: Riot_model.Lockfile.t option;
  workspace: Riot_model.Workspace.t;
}

let duration_ms_since = fun started ->
  Time.Instant.duration_since ~earlier:started (Time.Instant.now ()) |> Time.Duration.to_millis

type resolved_dependency = {
  dependency: Riot_model.Lockfile.dependency;
  packages: Riot_model.Lockfile.package list;
}

type resolution_state = {
  resolving: (string * Riot_model.Lockfile.package_id) list;
  resolved: (string * Riot_model.Lockfile.package) list;
}

let package_id_of_local_package = fun (pkg: Riot_model.Package.t) ->
  Riot_model.Lockfile.{ registry = None; name = pkg.name; version = None; sha256 = None }

let package_id_of_source_package = fun (pkg: Riot_model.Package.t) ->
  Riot_model.Lockfile.{
    registry = None;
    name = pkg.name;
    version = Option.map Std.Version.to_string pkg.publish.version;
    sha256 = None;
  }

let required_by_local_package = fun (pkg: Riot_model.Package.t) ->
  Riot_model.Pm_error.{ package = pkg.name; path = Some pkg.path }

let resolve_dependency_root = fun ~declared_from dep_path ->
  if Path.is_absolute dep_path then
    Path.normalize dep_path
  else
    Path.normalize Path.(declared_from / dep_path)

let relative_path_from = fun ~base path ->
  let base = Path.normalize base in
  let path = Path.normalize path in
  match Path.strip_prefix path ~prefix:base with
  | Ok relative -> relative
  | Error _ ->
      let rec drop_common base_parts path_parts =
        match base_parts, path_parts with
        | base_head :: base_rest, path_head :: path_rest when Path.equal base_head path_head -> drop_common
          base_rest
          path_rest
        | _ -> (base_parts, path_parts)
      in
      let rec build_path = function
        | [] -> Path.v "."
        | first :: rest -> List.fold_left Path.join first rest
      in
      let base_parts, path_parts = drop_common (Path.components base) (Path.components path) in
      let base_parts =
        List.filter (fun component -> not (Path.equal component (Path.v "/"))) base_parts
      in
      build_path ((List.map (fun _ -> Path.v "..") base_parts) @ path_parts)

let load_manifest_toml = fun ~manifest_path ->
  match Fs.read_to_string manifest_path with
  | Error err -> Error (Error.ManifestReadFailed { manifest_path; error = IO.error_message err })
  | Ok source -> (
      match Data.Toml.parse source with
      | Ok toml -> Ok toml
      | Error err -> Error (Error.ManifestParseFailed {
        manifest_path;
        error = Data.Toml.error_to_string err
      })
    )

let load_path_dependency_package = fun ~declared_from ~dependency_name dep_path ->
  let package_root = resolve_dependency_root ~declared_from dep_path in
  let manifest_path = Path.(package_root / Path.v "riot.toml") in
  match load_manifest_toml ~manifest_path with
  | Error err -> Error (Error.PathDependencyLoadFailed {
    dependency_name;
    dependency_path = dep_path;
    error = err
  })
  | Ok toml -> Riot_model.Package.from_toml
    toml
    ~workspace_deps:[]
    ~workspace_dev_deps:[]
    ~workspace_build_deps:[]
    ~path:package_root
    ~relative_path:dep_path
  |> Result.map_error
    (fun err -> Error.PathDependencyDecodeFailed { dependency_name; manifest_path; error = err })

let load_source_dependency_package = fun ~dependency_name ~source_locator ~ref_ ->
  let* materialized = Git_dependency.materialize ~source_locator ~ref_ ()
  |> Result.map_error (fun error -> Error.SourceDependencyLoadFailed {
    dependency_name;
    source_locator;
    ref_;
    error = Git_dependency.message error
  }) in
  let manifest_path = Path.(materialized.package_root / Path.v "riot.toml") in
  let* toml = load_manifest_toml ~manifest_path in
  let relative_path =
    match Path.strip_prefix materialized.package_root ~prefix:materialized.repository_root with
    | Ok relative_path -> relative_path
    | Error _ -> Path.v "."
  in
  Riot_model.Package.from_toml
    toml
    ~workspace_deps:[]
    ~workspace_dev_deps:[]
    ~workspace_build_deps:[]
    ~path:materialized.package_root
    ~relative_path
  |> Result.map_error
    (fun err -> Error.SourceDependencyDecodeFailed { dependency_name; manifest_path; error = err })

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
  registry ^ ":" ^ id.name ^ ":" ^ version

let registry_resolution_key = fun ~registry_name ~package_name ->
  registry_name ^ ":" ^ Pkgs_ml.Sparse_index.normalized_name package_name

let path_resolution_key = fun ~package_root -> "path:" ^ Path.to_string (Path.normalize package_root)

let source_resolution_key = fun ~source_locator ~ref_ ->
  "source:" ^ source_locator ^ "#"
  ^ Option.unwrap_or ~default:"main" ref_

let find_workspace_package_by_name = fun ~(workspace_packages:Riot_model.Package.t list) ~package_name ->
  List.find_opt
    (fun (pkg: Riot_model.Package.t) ->
      String.equal pkg.name package_name)
    workspace_packages

let find_workspace_package_by_root = fun ~(workspace_packages:Riot_model.Package.t list) ~package_root ->
  let package_root = Path.normalize package_root in
  List.find_opt
    (fun (pkg: Riot_model.Package.t) ->
      Path.equal (Path.normalize pkg.path) package_root)
    workspace_packages

let empty_resolution_state = { resolving = []; resolved = [] }

let find_resolving_package_id = fun ~(state:resolution_state) ~key ->
  List.assoc_opt key state.resolving

let find_resolved_package = fun ~(state:resolution_state) ~key ->
  List.assoc_opt key state.resolved

let find_local_package_id_in_state = fun ~(state:resolution_state) ~package_name ->
  match List.find_opt
    (fun (_, (pkg: Riot_model.Lockfile.package)) ->
      pkg.id.registry = None && String.equal pkg.id.name package_name)
    state.resolved with
  | Some (_, pkg) -> Some pkg.id
  | None -> List.find_opt
    (fun (_, (package_id: Riot_model.Lockfile.package_id)) ->
      package_id.registry = None && String.equal package_id.name package_name)
    state.resolving
  |> Option.map snd

let add_resolving = fun ~(state:resolution_state) ~key ~package_id ->
  { state with resolving = (key, package_id) :: state.resolving }

let remove_resolving = fun ~(state:resolution_state) ~key ->
  {
    state
    with resolving = List.filter (fun ((candidate, _)) -> not (String.equal candidate key)) state.resolving
  }

let add_resolved = fun ~(state:resolution_state) ~key ~(pkg:Riot_model.Lockfile.package) ->
  let state = remove_resolving ~state ~key in
  { state with resolved = (key, pkg) :: state.resolved }

let materialized_root_for_registry_package = fun ~registry ~package_name ~version ->
  Pkgs_ml.Registry_cache.package_src_dir (Pkgs_ml.Registry.cache registry) ~package_name ~version

let find_existing_external_package = fun ~registry_name ~existing_lock ~package_name ->
  match existing_lock with
  | None -> None
  | Some (lockfile: Riot_model.Lockfile.t) -> List.find_opt
    (fun (pkg: Riot_model.Lockfile.package) ->
      pkg.id.registry = Some registry_name && String.equal pkg.id.name package_name)
    lockfile.packages

let latest_release_of_document = fun (document: Pkgs_ml.Sparse_index.package_document) ->
  match
    List.find_opt
      (fun (release: Pkgs_ml.Sparse_index.release) ->
        String.equal release.version document.latest)
      document.releases
  with
  | Some release -> Ok release
  | None -> Error (Error.RegistryLatestReleaseMissing {
    package = document.name;
    latest_version = document.latest
  })

let lock_dependency_of_local_dependency = fun (dep: Riot_model.Package.dependency) ->
  Riot_model.Lockfile.{
    name = dep.name;
    package = { registry = None; name = dep.name; version = None; sha256 = None }
  }

let merge_lock_packages = fun packages ->
  let rec loop seen acc = function
    | [] -> List.rev acc
    | (pkg: Riot_model.Lockfile.package) :: rest ->
        let key = package_id_key pkg.id in
        if List.mem key seen then
          loop seen acc rest
        else
          loop (key :: seen) (pkg :: acc) rest
  in
  loop [] [] packages

let dependency_counts = fun (packages: Riot_model.Lockfile.package list) ->
  List.fold_left
    (fun (runtime, build, dev) (pkg: Riot_model.Lockfile.package) ->
      (
        runtime + List.length pkg.dependencies,
        build + List.length pkg.build_dependencies,
        dev + List.length pkg.dev_dependencies
      ))
    (0, 0, 0)
    packages

let rec lock_package_of_local_package = fun ~(ctx:context) ~state ~provenance (
  pkg: Riot_model.Package.t
) ->
  let required_by = Some (required_by_local_package pkg) in
  match resolve_manifest_dependencies ~ctx ~state ~required_by ~declared_from:pkg.path [] [] pkg.dependencies with
  | Error _ as err -> err
  | Ok (dependencies, dependency_packages, state) -> (
      match resolve_manifest_dependencies
        ~ctx
        ~state
        ~required_by
        ~declared_from:pkg.path
        []
        []
        pkg.build_dependencies with
      | Error _ as err -> err
      | Ok (build_dependencies, build_packages, state) -> (
          match resolve_manifest_dependencies
            ~ctx
            ~state
            ~required_by
            ~declared_from:pkg.path
            []
            []
            pkg.dev_dependencies with
          | Error _ as err -> err
          | Ok (dev_dependencies, dev_packages, state) ->
              let id =
                match provenance with
                | Riot_model.Lockfile.Source _ -> package_id_of_source_package pkg
                | _ -> package_id_of_local_package pkg
              in
              let root =
                match provenance with
                | Riot_model.Lockfile.Source _ -> None
                | _ -> Some (relative_path_from ~base:ctx.workspace.root pkg.path)
              in
              Ok (
                Riot_model.Lockfile.{
                  id;
                  root;
                  provenance;
                  dependencies;
                  build_dependencies;
                  dev_dependencies;
                },
                dependency_packages @ build_packages @ dev_packages,
                state
              )
        )
    )

and resolve_registry_dependency = fun ~(ctx:context) ~state ~required_by (
  dep: Riot_model.Package.dependency
) ->
  let package_name = dep.name in
  let registry_name = Pkgs_ml.Registry.name ctx.registry in
  match ctx.mode, find_existing_external_package ~registry_name ~existing_lock:ctx.existing_lock ~package_name with
  | Refresh, Some (existing_pkg: Riot_model.Lockfile.package) ->
      Ok (
        {
          dependency =
            Riot_model.Lockfile.{ name = package_name; package = existing_pkg.id };
          packages = [];
        },
        state
      )
  | _ ->
      let key = registry_resolution_key ~registry_name ~package_name in
      match find_resolved_package ~state ~key with
      | Some lock_package ->
          Ok (
            {
              dependency =
                Riot_model.Lockfile.{ name = package_name; package = lock_package.id };
              packages = [];
            },
            state
          )
      | None -> (
          match find_resolving_package_id ~state ~key with
          | Some package_id ->
              Ok (
                {
                  dependency =
                    Riot_model.Lockfile.{ name = package_name; package = package_id };
                  packages = [];
                },
                state
              )
          | None -> (
              ctx.emit
                (Riot_model.Event.RegistryIndexUpdating {
                  registry = Pkgs_ml.Registry.name ctx.registry
                });
              let metadata_started = Time.Instant.now () in
              ctx.emit
                (Riot_model.Event.PackageMetadataFetchStarted {
                  registry = registry_name;
                  package = package_name
                });
              match Pkgs_ml.Registry.read_package_document ctx.registry ~package_name with
              | Error err ->
                  let error = Error.PackageMetadataReadFailed {
                    package = package_name;
                    registry = registry_name;
                    error = err
                  } in
                  ctx.emit
                    (Riot_model.Event.PackageMetadataFetchFailed {
                      registry = registry_name;
                      package = package_name;
                      error
                    });
                  Error error
              | Ok None ->
                  let error = Error.PackageNotFound {
                    package = package_name;
                    registry = registry_name;
                    required_by
                  } in
                  ctx.emit
                    (Riot_model.Event.PackageMetadataFetchFailed {
                      registry = registry_name;
                      package = package_name;
                      error
                    });
                  Error error
              | Ok (Some document) -> (
                  ctx.emit
                    (Riot_model.Event.PackageMetadataFetchFinished {
                      registry = registry_name;
                      package = document.name;
                      version = Some document.latest;
                      duration_ms = duration_ms_since metadata_started
                    });
                  match latest_release_of_document document with
                  | Error _ as err -> err
                  | Ok (release: Pkgs_ml.Sparse_index.release) ->
                      let package_id =
                        Riot_model.Lockfile.{
                          registry = Some registry_name;
                          name = document.name;
                          version = Some release.version;
                          sha256 = Some release.artifact_sha256
                        } in
                      let state = add_resolving ~state ~key ~package_id in
                      let rec resolve_release_dependencies ~(state:resolution_state) (
                        acc_packages: Riot_model.Lockfile.package list
                      ) (acc_dependencies: Riot_model.Lockfile.dependency list) (
                        release_dependencies: Pkgs_ml.Sparse_index.dependency list
                      ) =
                        match release_dependencies with
                        | [] -> Ok (List.rev acc_dependencies, acc_packages, state)
                        | (dep: Pkgs_ml.Sparse_index.dependency) :: rest ->
                            if Riot_model.Package.is_builtin_dependency_name dep.name then
                              resolve_release_dependencies ~state acc_packages acc_dependencies rest
                            else
                              (
                                match resolve_registry_dependency
                                  ~ctx
                                  ~state
                                  ~required_by:(Some Riot_model.Pm_error.{
                                    package = document.name;
                                    path = None
                                  })
                                  Riot_model.Package.{
                                    name = dep.name;
                                    source = {
                                      workspace = false;
                                      builtin = false;
                                      path = None;
                                      source_locator = None;
                                      ref_ = None;
                                      version = None
                                    }
                                  } with
                                | Error _ as err -> err
                                | Ok (resolved, state) -> resolve_release_dependencies
                                  ~state
                                  (List.rev_append resolved.packages acc_packages)
                                  (resolved.dependency :: acc_dependencies)
                                  rest
                              )
                      in
                      match resolve_release_dependencies ~state [] [] release.dependencies with
                      | Error _ as err -> err
                      | Ok (dependencies, dependency_packages, state) ->
                          let lock_package =
                            Riot_model.Lockfile.{
                              id = package_id;
                              root = None;
                              provenance = Registry { registry = registry_name };
                              dependencies;
                              build_dependencies = [];
                              dev_dependencies = [];
                            }
                          in
                          let state = add_resolved ~state ~key ~pkg:lock_package in
                          Ok (
                            {
                              dependency =
                                Riot_model.Lockfile.{
                                  name = package_name;
                                  package = lock_package.id
                                };
                              packages = dependency_packages @ [ lock_package ];
                            },
                            state
                          )
                )
            )
        )

and resolve_path_dependency = fun ~(ctx:context) ~state ~declared_from dependency_name dep_path ->
  let package_root = resolve_dependency_root ~declared_from dep_path in
  let key = path_resolution_key ~package_root in
  match find_workspace_package_by_root ~workspace_packages:ctx.workspace.packages ~package_root with
  | Some workspace_pkg ->
      Ok (
        {
          dependency =
            Riot_model.Lockfile.{
              name = dependency_name;
              package = package_id_of_local_package workspace_pkg
            };
          packages = [];
        },
        state
      )
  | None -> (
      match find_resolved_package ~state ~key with
      | Some lock_package ->
          Ok (
            {
              dependency =
                Riot_model.Lockfile.{ name = dependency_name; package = lock_package.id };
              packages = [];
            },
            state
          )
      | None -> (
          match find_resolving_package_id ~state ~key with
          | Some package_id ->
              Ok (
                {
                  dependency =
                    Riot_model.Lockfile.{ name = dependency_name; package = package_id };
                  packages = [];
                },
                state
              )
          | None -> (
              match load_path_dependency_package ~declared_from ~dependency_name dep_path with
              | Error _ as err -> err
              | Ok pkg -> (
                  match find_workspace_package_by_name
                    ~workspace_packages:ctx.workspace.packages
                    ~package_name:pkg.name with
                  | Some workspace_pkg ->
                      Ok (
                        {
                          dependency =
                            Riot_model.Lockfile.{
                              name = dependency_name;
                              package = package_id_of_local_package workspace_pkg
                            };
                          packages = [];
                        },
                        state
                      )
                  | None ->
                      let package_id = package_id_of_local_package pkg in
                      let state = add_resolving ~state ~key ~package_id in
                      match lock_package_of_local_package
                        ~ctx
                        ~state
                        ~provenance:(Riot_model.Lockfile.Path dep_path)
                        pkg with
                      | Error _ as err -> err
                      | Ok (lock_package, dependency_packages, state) ->
                          let state = add_resolved ~state ~key ~pkg:lock_package in
                          Ok (
                            {
                              dependency =
                                Riot_model.Lockfile.{
                                  name = dependency_name;
                                  package = lock_package.id
                                };
                              packages = dependency_packages @ [ lock_package ];
                            },
                            state
                          )
                )
            )
        )
    )

and resolve_source_dependency = fun ~(ctx:context) ~state (dep: Riot_model.Package.dependency) ->
  let dependency_name = dep.name in
  let source_locator =
    match dep.source.source_locator with
    | Some source_locator -> source_locator
    | None -> panic "resolve_source_dependency requires a source locator"
  in
  let ref_ = dep.source.ref_ in
  let key = source_resolution_key ~source_locator ~ref_ in
  match find_resolved_package ~state ~key with
  | Some lock_package ->
      Ok (
        {
          dependency =
            Riot_model.Lockfile.{ name = dependency_name; package = lock_package.id };
          packages = [];
        },
        state
      )
  | None -> (
      match find_resolving_package_id ~state ~key with
      | Some package_id ->
          Ok (
            {
              dependency =
                Riot_model.Lockfile.{ name = dependency_name; package = package_id };
              packages = [];
            },
            state
          )
      | None -> (
          let* pkg = load_source_dependency_package ~dependency_name ~source_locator ~ref_ in
          let* () =
            match dep.source.version, pkg.publish.version with
            | Some requirement, Some version when Std.Version.matches requirement version ->
                Ok ()
            | Some requirement, Some version ->
                Error (Error.Unexpected {
                  error = "source dependency '"
                  ^ dependency_name
                  ^ "' from '"
                  ^ source_locator
                  ^ "' does not satisfy required version '"
                  ^ Std.Version.requirement_to_string requirement
                  ^ "' (found "
                  ^ Std.Version.to_string version
                  ^ ")"
                })
            | Some requirement, None ->
                Error (Error.Unexpected {
                  error = "source dependency '"
                  ^ dependency_name
                  ^ "' from '"
                  ^ source_locator
                  ^ "' is missing a package version required by '"
                  ^ Std.Version.requirement_to_string requirement
                  ^ "'"
                })
            | None, _ ->
                Ok ()
          in
          let package_id = package_id_of_source_package pkg in
          let state = add_resolving ~state ~key ~package_id in
          match lock_package_of_local_package
            ~ctx
            ~state
            ~provenance:(Riot_model.Lockfile.Source { locator = source_locator; ref_ })
            pkg with
          | Error _ as err -> err
          | Ok (lock_package, dependency_packages, state) ->
              let state = add_resolved ~state ~key ~pkg:lock_package in
              Ok (
                {
                  dependency =
                    Riot_model.Lockfile.{
                      name = dependency_name;
                      package = lock_package.id
                    };
                  packages = dependency_packages @ [ lock_package ];
                },
                state
              )
        )
    )

and resolve_manifest_dependencies = fun ~(ctx:context) ~state ~required_by ~declared_from acc_packages acc_dependencies deps ->
  match deps with
  | [] -> Ok (List.rev acc_dependencies, List.rev acc_packages, state)
  | dep :: rest -> (
      match dep.Riot_model.Package.source with
      | { workspace=true; _ } ->
          resolve_manifest_dependencies
            ~ctx
            ~state
            ~required_by
            ~declared_from
            acc_packages
            (lock_dependency_of_local_dependency dep :: acc_dependencies)
            rest
      | { builtin=true; _ } ->
          resolve_manifest_dependencies
            ~ctx
            ~state
            ~required_by
            ~declared_from
            acc_packages
            acc_dependencies
            rest
      | { path=Some path; _ } -> (
          match resolve_path_dependency ~ctx ~state ~declared_from dep.name path with
          | Error _ as err -> err
          | Ok (resolved, state) -> resolve_manifest_dependencies
            ~ctx
            ~state
            ~required_by
            ~declared_from
            (List.rev_append resolved.packages acc_packages)
            (resolved.dependency :: acc_dependencies)
            rest
        )
      | { source_locator=Some _; _ } -> (
          match resolve_source_dependency ~ctx ~state dep with
          | Error _ as err -> err
          | Ok (resolved, state) -> resolve_manifest_dependencies
            ~ctx
            ~state
            ~required_by
            ~declared_from
            (List.rev_append resolved.packages acc_packages)
            (resolved.dependency :: acc_dependencies)
            rest
        )
      | { path=None; _ } -> (
          match find_workspace_package_by_name
            ~workspace_packages:ctx.workspace.packages
            ~package_name:dep.name with
          | Some _ -> resolve_manifest_dependencies
            ~ctx
            ~state
            ~required_by
            ~declared_from
            acc_packages
            (lock_dependency_of_local_dependency dep :: acc_dependencies)
            rest
          | None -> (
              match find_local_package_id_in_state ~state ~package_name:dep.name with
              | Some package_id -> resolve_manifest_dependencies
                ~ctx
                ~state
                ~required_by
                ~declared_from
                acc_packages
                (Riot_model.Lockfile.{ name = dep.name; package = package_id } :: acc_dependencies)
                rest
              | None -> (
                  match resolve_registry_dependency ~ctx ~state ~required_by dep with
                  | Error _ as err -> err
                  | Ok (resolved, state) -> resolve_manifest_dependencies
                    ~ctx
                    ~state
                    ~required_by
                    ~declared_from
                    (List.rev_append resolved.packages acc_packages)
                    (resolved.dependency :: acc_dependencies)
                    rest
                )
            )
        )
    )

let lock_package_of_workspace_package = fun ~(ctx:context) ~state (pkg: Riot_model.Package.t) ->
  lock_package_of_local_package ~ctx ~state ~provenance:Riot_model.Lockfile.Workspace pkg

let rec lock_packages = fun ~(ctx:context) ~state acc_workspace acc_external packages ->
  match packages with
  | [] -> Ok (List.rev acc_workspace, List.rev acc_external, state)
  | pkg :: rest -> (
      match lock_package_of_workspace_package ~ctx ~state pkg with
      | Ok (pkg, external_packages, state) -> lock_packages
        ~ctx
        ~state
        (pkg :: acc_workspace)
        (List.rev_append external_packages acc_external)
        rest
      | Error _ as err -> err
    )

let keep_existing_package = fun workspace_packages (pkg: Riot_model.Lockfile.package) ->
  let workspace_names =
    List.map (fun (pkg: Riot_model.Package.t) -> pkg.name) workspace_packages
  in
  not (List.mem pkg.id.name workspace_names)

let resolve_root_scope = fun ~(ctx:context) ~state ~(declared_from:Path.t) deps ->
  match resolve_manifest_dependencies ~ctx ~state ~required_by:None ~declared_from [] [] deps with
  | Error _ as err -> err
  | Ok (dependencies, packages, state) -> Ok (dependencies, packages, state)

let lock_root_dependencies = fun ~(ctx:context) ~state ->
  let declared_from = ctx.workspace.root in
  match resolve_root_scope ~ctx ~state ~declared_from ctx.workspace.dependencies with
  | Error _ as err -> err
  | Ok (runtime_dependencies, runtime_packages, state) -> (
      match resolve_root_scope ~ctx ~state ~declared_from ctx.workspace.build_dependencies with
      | Error _ as err -> err
      | Ok (build_dependencies, build_packages, state) -> (
          match resolve_root_scope ~ctx ~state ~declared_from ctx.workspace.dev_dependencies with
          | Error _ as err -> err
          | Ok (dev_dependencies, dev_packages, state) -> Ok (
            runtime_dependencies,
            build_dependencies,
            dev_dependencies,
            runtime_packages @ build_packages @ dev_packages,
            state
          )
        )
    )

let collect_reachable_existing = fun ~(existing_lock:Riot_model.Lockfile.t) ~(root_ids:Riot_model.Lockfile.package_id list) ->
  let find_package package_id =
    List.find_opt (fun (pkg: Riot_model.Lockfile.package) -> pkg.id = package_id) existing_lock.packages
  in
  let rec loop seen acc pending =
    match pending with
    | [] -> List.rev acc
    | package_id :: rest ->
        let key = package_id_key package_id in
        if List.mem key seen then
          loop seen acc rest
        else
          match find_package package_id with
          | None -> loop (key :: seen) acc rest
          | Some pkg ->
              let next = List.map (fun (dep: Riot_model.Lockfile.dependency) -> dep.package) pkg.dependencies
              @ List.map (fun (dep: Riot_model.Lockfile.dependency) -> dep.package) pkg.build_dependencies
              @ List.map (fun (dep: Riot_model.Lockfile.dependency) -> dep.package) pkg.dev_dependencies in
              loop (key :: seen) (pkg :: acc) (next @ rest)
  in
  loop [] [] root_ids

let lock_deps = fun ?(emit = no_emit) ~mode ~registry ~existing_lock ~workspace () ->
  let started = Time.Instant.now () in
  let ctx = {
    emit;
    mode;
    registry;
    existing_lock;
    workspace;
  }
  in
  let packages = List.filter Riot_model.Package.is_workspace_member workspace.packages in
  emit
    (Riot_model.Event.DependencyUniverseBuilding {
      packages = List.map (fun (pkg: Riot_model.Package.t) -> pkg.name) packages
    });
  match lock_packages ~ctx ~state:empty_resolution_state [] [] packages with
  | Ok (workspace_packages, external_packages, state) -> (
      match lock_root_dependencies ~ctx ~state with
      | Error _ as err -> err
      | Ok (root_runtime_dependencies, root_build_dependencies, root_dev_dependencies, root_external_packages, _state) ->
          let preserved =
            match (mode, existing_lock) with
            | Unlock, _ ->
                []
            | Refresh, Some (existing_lock: Riot_model.Lockfile.t) ->
                existing_lock.packages
                |> List.filter (keep_existing_package packages)
            | Refresh, None ->
                []
          in
          let packages = merge_lock_packages
            (workspace_packages @ external_packages @ root_external_packages @ preserved) in
          let runtime_packages, build_packages, dev_packages = dependency_counts packages in
          emit
            (Riot_model.Event.DependencyUniverseBuilt {
              runtime_packages;
              build_packages;
              dev_packages;
              duration_ms = duration_ms_since started
            });
          Ok Riot_model.Lockfile.{ format_version = 1; packages }
    )
  | Error _ as err -> err
