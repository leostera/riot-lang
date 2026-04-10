open Std
open Std.Collections
module Error = Error

let ( let* ) = Result.and_then

type mode =
  | Refresh
  | Unlock

type event_sink = Riot_model.Event.kind -> unit

let no_emit: event_sink = fun _ -> ()

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
    sha256 = None
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

let path_dependency_has_external_fallback = fun (dep: Riot_model.Package.dependency) ->
  Option.is_some dep.source.source_locator || Option.is_some dep.source.version

type path_dependency_resolution =
  | PreferLocalPath
  | FallbackToSource
  | FallbackToRegistry

let path_dependency_resolution = fun ~declared_from (dep: Riot_model.Package.dependency) ->
  match dep.source.path with
  | None -> None
  | Some dep_path ->
      let manifest_path = Path.(resolve_dependency_root ~declared_from dep_path / Path.v "riot.toml") in
      match Fs.exists manifest_path with
      | Ok false when Option.is_some dep.source.source_locator -> Some FallbackToSource
      | Ok false when path_dependency_has_external_fallback dep -> Some FallbackToRegistry
      | _ -> Some PreferLocalPath

let load_source_dependency_package = fun ~dependency_name ~source_locator ~ref_ ->
  let* materialized = Git_dependency.materialize ~source_locator ~ref_ ()
  |> Result.map_error
    (fun error ->
      Error.SourceDependencyLoadFailed {
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
  "source:" ^ source_locator ^ "#" ^ Option.unwrap_or ~default:"main" ref_

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
  | Some release when release.yanked -> Error (Error.RegistryReleaseYanked {
    package = document.name;
    registry = "pkgs.ml";
    version = release.version;
    required_by = None
  })
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
                                match
                                  resolve_registry_dependency ~ctx ~state ~required_by:(Some Riot_model.Pm_error.{
                                    package = document.name;
                                    path = None
                                  })
                                    Riot_model.Package.{
                                      name = dep.name;
                                      source =
                                        {
                                          workspace = false;
                                          builtin = false;
                                          path = None;
                                          source_locator = None;
                                          ref_ = None;
                                          version = None;
                                        };
                                    }
                                with
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
            | Some requirement, Some version when Std.Version.matches requirement version -> Ok ()
            | Some requirement, Some version -> Error (Error.Unexpected {
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
            | Some requirement, None -> Error (Error.Unexpected {
              error = "source dependency '"
              ^ dependency_name
              ^ "' from '"
              ^ source_locator
              ^ "' is missing a package version required by '"
              ^ Std.Version.requirement_to_string requirement
              ^ "'"
            })
            | None, _ -> Ok ()
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
                    Riot_model.Lockfile.{ name = dependency_name; package = lock_package.id };
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
          match path_dependency_resolution ~declared_from dep with
          | Some PreferLocalPath -> (
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
          | Some FallbackToSource -> (
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
          | Some FallbackToRegistry
          | None -> (
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

let pubgrub_root_package = "__riot_lock_root__"

let pubgrub_root_version = Std.Version.make ~major:0 ~minor:0 ~patch:0 ()

let pubgrub_version_compare = fun left right ->
  match Std.Version.compare left right with
  | Lt -> (-1)
  | Eq -> 0
  | Gt -> 1

type local_entry = {
  package: Riot_model.Package.t;
  provenance: Riot_model.Lockfile.provenance;
}

type catalog = {
  ctx: context;
  local_by_name: (string, local_entry) HashMap.t;
  mutable local_order: string list;
  registry_documents: (string, Pkgs_ml.Sparse_index.package_document option) HashMap.t;
  required_by: (string, Riot_model.Pm_error.required_by option) HashMap.t;
  requested_requirements: (string, string list) HashMap.t;
}

let local_solver_version = fun (pkg: Riot_model.Package.t) ->
  match pkg.publish.version with
  | Some version -> version
  | None -> pubgrub_root_version

let package_id_of_local_entry = fun (entry: local_entry) ->
  match entry.provenance with
  | Riot_model.Lockfile.Source _ -> package_id_of_source_package entry.package
  | Riot_model.Lockfile.Workspace
  | Riot_model.Lockfile.Path _ -> package_id_of_local_package entry.package
  | Riot_model.Lockfile.Registry _ -> panic "local catalog entries cannot use registry provenance"

let lock_root_of_local_entry = fun ~workspace_root (entry: local_entry) ->
  match entry.provenance with
  | Riot_model.Lockfile.Source _ -> None
  | Riot_model.Lockfile.Workspace
  | Riot_model.Lockfile.Path _ -> Some (relative_path_from ~base:workspace_root entry.package.path)
  | Riot_model.Lockfile.Registry _ -> None

let create_catalog = fun ~(ctx:context) ->
  {
    ctx;
    local_by_name = HashMap.create ();
    local_order = [];
    registry_documents = HashMap.create ();
    required_by = HashMap.create ();
    requested_requirements = HashMap.create ();
  }

let register_local_entry = fun (catalog: catalog) (entry: local_entry) ->
  match HashMap.get catalog.local_by_name entry.package.name with
  | Some existing ->
      if Path.equal existing.package.path entry.package.path then
        Ok ()
      else
        Error (Error.Unexpected {
          error = "multiple local packages named '"
          ^ entry.package.name
          ^ "' were discovered at "
          ^ Path.to_string existing.package.path
          ^ " and "
          ^ Path.to_string entry.package.path
        })
  | None ->
      let _ = HashMap.insert catalog.local_by_name entry.package.name entry in
      catalog.local_order <- catalog.local_order @ [ entry.package.name ];
      Ok ()

let register_workspace_packages = fun (catalog: catalog) packages ->
  let rec loop = function
    | [] -> Ok ()
    | (pkg: Riot_model.Package.t) :: rest ->
        let* () = register_local_entry
          catalog
          { package = pkg; provenance = Riot_model.Lockfile.Workspace } in
        loop rest
  in
  loop packages

let ranges_of_requirement = fun requirement ->
  match Std.Version.view_requirement requirement with
  | Std.Version.AnyRequirement ->
      Ok Pubgrub.full
  | Std.Version.PrefixMajorRequirement major ->
      let lower_bound = Std.Version.make ~major ~minor:0 ~patch:0 () in
      let upper_bound = Std.Version.make ~major:(major + 1) ~minor:0 ~patch:0 () in
      Ok (Pubgrub.Ranges.intersection
        ~compare_v:pubgrub_version_compare
        (Pubgrub.higher_than lower_bound)
        (Pubgrub.strictly_lower_than upper_bound))
  | Std.Version.PrefixMinorRequirement (major, minor) ->
      let lower_bound = Std.Version.make ~major ~minor ~patch:0 () in
      let upper_bound = Std.Version.make ~major ~minor:(minor + 1) ~patch:0 () in
      Ok (Pubgrub.Ranges.intersection
        ~compare_v:pubgrub_version_compare
        (Pubgrub.higher_than lower_bound)
        (Pubgrub.strictly_lower_than upper_bound))
  | Std.Version.ExactRequirement version ->
      Ok (Pubgrub.singleton version)
  | Std.Version.NotEqualRequirement version ->
      Ok (Pubgrub.Ranges.complement ~compare_v:pubgrub_version_compare (Pubgrub.singleton version))
  | Std.Version.GreaterThanRequirement version ->
      Ok (Pubgrub.strictly_higher_than version)
  | Std.Version.GreaterThanOrEqualRequirement version ->
      Ok (Pubgrub.higher_than version)
  | Std.Version.LessThanRequirement version ->
      Ok (Pubgrub.strictly_lower_than version)
  | Std.Version.LessThanOrEqualRequirement version ->
      Ok (Pubgrub.lower_than version)
  | Std.Version.TildeRequirement version ->
      let upper_bound = Std.Version.make ~major:version.major ~minor:(version.minor + 1) ~patch:0 () in
      Ok (Pubgrub.Ranges.intersection
        ~compare_v:pubgrub_version_compare
        (Pubgrub.higher_than version)
        (Pubgrub.strictly_lower_than upper_bound))

let record_required_by = fun (catalog: catalog) ~package_name required_by ->
  match required_by with
  | None -> ()
  | Some required_by ->
      if String.equal package_name pubgrub_root_package then
        ()
      else
        match HashMap.get catalog.required_by package_name with
        | Some _ -> ()
        | None ->
            let _ = HashMap.insert catalog.required_by package_name (Some required_by) in
            ()

let required_by_for_package = fun (catalog: catalog) package_name ->
  match HashMap.get catalog.required_by package_name with
  | Some required_by -> required_by
  | None -> None

let record_requested_requirement = fun (catalog: catalog) ~package_name requirement ->
  let requirement = Std.Version.requirement_to_string requirement in
  match HashMap.get catalog.requested_requirements package_name with
  | Some existing when List.mem requirement existing ->
      ()
  | Some existing ->
      let _ = HashMap.insert catalog.requested_requirements package_name (existing @ [ requirement ]) in
      ()
  | None ->
      let _ = HashMap.insert catalog.requested_requirements package_name [ requirement ] in
      ()

let requested_requirement_for_package = fun (catalog: catalog) package_name ->
  match HashMap.get catalog.requested_requirements package_name with
  | Some [] -> None
  | Some [ requirement ] -> Some requirement
  | Some requirements -> Some (String.concat ", " requirements)
  | None -> None

let validate_source_requirement = fun ~dependency_name ~source_locator (
  dep: Riot_model.Package.dependency
) (pkg: Riot_model.Package.t) ->
  match dep.source.version, pkg.publish.version with
  | Some requirement, Some version when Std.Version.matches requirement version -> Ok ()
  | Some requirement, Some version -> Error (Error.Unexpected {
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
  | Some requirement, None -> Error (Error.Unexpected {
    error = "source dependency '"
    ^ dependency_name
    ^ "' from '"
    ^ source_locator
    ^ "' is missing a package version required by '"
    ^ Std.Version.requirement_to_string requirement
    ^ "'"
  })
  | None, _ -> Ok ()

let dependency_target_name = fun (catalog: catalog) ~declared_from ~required_by (
  dep: Riot_model.Package.dependency
) ->
  match dep.source with
  | { builtin=true; _ } ->
      Ok None
  | { workspace=true; _ } ->
      Ok (Some dep.name)
  | { path=Some path; _ } -> (
      match path_dependency_resolution ~declared_from dep with
      | Some PreferLocalPath ->
          let package_root = resolve_dependency_root ~declared_from path in
          (
            match find_workspace_package_by_root
              ~workspace_packages:catalog.ctx.workspace.packages
              ~package_root with
            | Some workspace_pkg -> Ok (Some workspace_pkg.name)
            | None ->
                let* pkg = load_path_dependency_package ~declared_from ~dependency_name:dep.name path in
                (
                  match find_workspace_package_by_name
                    ~workspace_packages:catalog.ctx.workspace.packages
                    ~package_name:pkg.name with
                  | Some workspace_pkg -> Ok (Some workspace_pkg.name)
                  | None ->
                      let* () = register_local_entry
                        catalog
                        { package = pkg; provenance = Riot_model.Lockfile.Path path } in
                      Ok (Some pkg.name)
                )
          )
      | Some FallbackToSource -> (
          let source_locator =
            match dep.source.source_locator with
            | Some source_locator -> source_locator
            | None -> panic "path dependency source fallback requires a source locator"
          in
          let* pkg = load_source_dependency_package
            ~dependency_name:dep.name
            ~source_locator
            ~ref_:dep.source.ref_ in
          let* () = validate_source_requirement ~dependency_name:dep.name ~source_locator dep pkg in
          let* () = register_local_entry
            catalog
            {
              package = pkg;
              provenance = Riot_model.Lockfile.Source {
                locator = source_locator;
                ref_ = dep.source.ref_
              }
            } in
          Ok (Some pkg.name)
        )
      | Some FallbackToRegistry
      | None ->
          Ok (Some dep.name)
    )
  | { source_locator=Some source_locator; _ } ->
      let* pkg = load_source_dependency_package
        ~dependency_name:dep.name
        ~source_locator
        ~ref_:dep.source.ref_ in
      let* () = validate_source_requirement ~dependency_name:dep.name ~source_locator dep pkg in
      let* () = register_local_entry
        catalog
        {
          package = pkg;
          provenance = Riot_model.Lockfile.Source {
            locator = source_locator;
            ref_ = dep.source.ref_
          }
        } in
      Ok (Some pkg.name)
  | _ ->
      Ok (Some dep.name)

let provider_dependency_of_manifest_dependency = fun (catalog: catalog) ~declared_from ~required_by (
  dep: Riot_model.Package.dependency
) ->
  let* target_name_opt = dependency_target_name catalog ~declared_from ~required_by dep in
  match target_name_opt with
  | None -> Ok None
  | Some target_name ->
      let is_local =
        match dep.source with
        | { workspace=true; _ }
        | { source_locator=Some _; _ } ->
            true
        | { path=Some _; _ } -> (
            match path_dependency_resolution ~declared_from dep with
            | Some PreferLocalPath
            | Some FallbackToSource -> true
            | Some FallbackToRegistry
            | None -> (
                match HashMap.get catalog.local_by_name target_name with
                | Some _ -> true
                | None -> false
              )
          )
        | { builtin=true; _ } ->
            false
        | _ ->
            match HashMap.get catalog.local_by_name target_name with
            | Some _ -> true
            | None -> false
      in
      if is_local then
        Ok (Some (target_name, Pubgrub.full))
      else
        let () = record_required_by catalog ~package_name:target_name required_by in
        let () =
          match dep.source.version with
          | Some requirement -> record_requested_requirement catalog ~package_name:target_name requirement
          | None -> ()
        in
        let* ranges =
          match dep.source.version with
          | Some requirement -> ranges_of_requirement requirement
          | None -> Ok Pubgrub.full
        in
        Ok (Some (target_name, ranges))

let provider_dependencies_of_manifest = fun (catalog: catalog) ~declared_from ~required_by deps ->
  let rec loop acc = function
    | [] -> Ok (List.rev acc)
    | dep :: rest ->
        let* provider_dep_opt = provider_dependency_of_manifest_dependency
          catalog
          ~declared_from
          ~required_by
          dep in
        (
          match provider_dep_opt with
          | Some provider_dep -> loop (provider_dep :: acc) rest
          | None -> loop acc rest
        )
  in
  loop [] deps

let find_existing_registry_package = fun ~registry_name ~existing_lock ~package_name ->
  match existing_lock with
  | None -> None
  | Some (lockfile: Riot_model.Lockfile.t) -> List.find_opt
    (fun (pkg: Riot_model.Lockfile.package) ->
      pkg.id.registry = Some registry_name && String.equal pkg.id.name package_name)
    lockfile.packages

let find_existing_registry_package_version = fun ~registry_name ~existing_lock ~package_name ~version ->
  match existing_lock with
  | None -> None
  | Some (lockfile: Riot_model.Lockfile.t) -> List.find_opt
    (fun (pkg: Riot_model.Lockfile.package) ->
      pkg.id.registry = Some registry_name
      && String.equal pkg.id.name package_name
      && pkg.id.version = Some version)
    lockfile.packages

let read_registry_document = fun (catalog: catalog) ~package_name ->
  match HashMap.get catalog.registry_documents package_name with
  | Some document_opt -> Ok document_opt
  | None ->
      let registry_name = Pkgs_ml.Registry.name catalog.ctx.registry in
      catalog.ctx.emit (Riot_model.Event.RegistryIndexUpdating { registry = registry_name });
      let started = Time.Instant.now () in
      catalog.ctx.emit
        (Riot_model.Event.PackageMetadataFetchStarted {
          registry = registry_name;
          package = package_name
        });
      (
        match Pkgs_ml.Registry.read_package_document catalog.ctx.registry ~package_name with
        | Error err ->
            let error = Error.PackageMetadataReadFailed {
              package = package_name;
              registry = registry_name;
              error = err
            } in
            catalog.ctx.emit
              (Riot_model.Event.PackageMetadataFetchFailed {
                registry = registry_name;
                package = package_name;
                error
              });
            Error error
        | Ok document_opt ->
            let _ = HashMap.insert catalog.registry_documents package_name document_opt in
            (
              match document_opt with
              | Some document ->
                  catalog.ctx.emit
                    (Riot_model.Event.PackageMetadataFetchFinished {
                      registry = registry_name;
                      package = document.name;
                      version = Some document.latest;
                      duration_ms = duration_ms_since started
                    });
                  Ok document_opt
              | None ->
                  let error = Error.PackageNotFound {
                    package = package_name;
                    registry = registry_name;
                    required_by = required_by_for_package catalog package_name
                  } in
                  catalog.ctx.emit
                    (Riot_model.Event.PackageMetadataFetchFailed {
                      registry = registry_name;
                      package = package_name;
                      error
                    });
                  Ok None
            )
      )

let parse_registry_version = fun ~package_name version_string ->
  Std.Version.parse version_string
  |> Result.map_error
    (fun _ ->
      Error.Unexpected {
        error = "failed to parse registry version '"
        ^ version_string
        ^ "' for package '"
        ^ package_name
        ^ "'"
      })

let sort_registry_versions = fun ~package_name versions ->
  let compare left right =
    match parse_registry_version ~package_name left, parse_registry_version ~package_name right with
    | Ok left, Ok right -> pubgrub_version_compare left right
    | _ -> String.compare left right
  in
  List.sort compare versions

let matching_registry_versions = fun (catalog: catalog) ~package_name ~ranges ->
  let rec contains_version version = function
    | [] -> false
    | existing :: rest ->
        if Std.Version.equal existing version then
          true
        else
          contains_version version rest
  in
  let registry_name = Pkgs_ml.Registry.name catalog.ctx.registry in
  let add_matching existing version =
    if
      Pubgrub.Ranges.contains ~compare_v:pubgrub_version_compare ranges version
      && not (contains_version version existing)
    then
      version :: existing
    else
      existing
  in
  let versions =
    match catalog.ctx.mode with
    | Unlock -> []
    | Refresh -> (
        match find_existing_registry_package
          ~registry_name
          ~existing_lock:catalog.ctx.existing_lock
          ~package_name with
        | Some pkg -> (
            match pkg.id.version with
            | Some version_string -> (
                match parse_registry_version ~package_name version_string with
                | Ok version -> add_matching [] version
                | Error _ -> []
              )
            | None -> []
          )
        | None -> []
      )
  in
  let* document_opt = read_registry_document catalog ~package_name in
  match document_opt with
  | None -> Ok versions
  | Some document ->
      let rec loop acc = function
        | [] -> Ok acc
        | (release: Pkgs_ml.Sparse_index.release) :: rest ->
            if release.yanked then
              loop acc rest
            else
              let* version = parse_registry_version ~package_name release.version in
              loop (add_matching acc version) rest
      in
      loop versions document.releases

let highest_version = fun versions ->
  List.sort (fun left right -> pubgrub_version_compare right left) versions |> List.hd

let registry_provider_dependencies_of_locked_dependency = fun (dep: Riot_model.Lockfile.dependency) ->
  match dep.package.registry, dep.package.version with
  | Some _, Some version_string ->
      let* version = parse_registry_version ~package_name:dep.package.name version_string in
      Ok (dep.package.name, Pubgrub.singleton version)
  | _ -> Ok (dep.package.name, Pubgrub.full)

let provider_dependencies_of_registry_package = fun (catalog: catalog) ~package_name version ->
  let registry_name = Pkgs_ml.Registry.name catalog.ctx.registry in
  let version_string = Std.Version.to_string version in
  match find_existing_registry_package_version
    ~registry_name
    ~existing_lock:catalog.ctx.existing_lock
    ~package_name
    ~version:version_string with
  | Some pkg when catalog.ctx.mode = Refresh ->
      let rec loop acc = function
        | [] -> Ok (List.rev acc)
        | dep :: rest ->
            let* provider_dep = registry_provider_dependencies_of_locked_dependency dep in
            loop (provider_dep :: acc) rest
      in
      loop [] (pkg.dependencies @ pkg.build_dependencies @ pkg.dev_dependencies)
      |> Result.map (fun deps -> Pubgrub.Provider.Available deps)
  | _ ->
      let* document_opt = read_registry_document catalog ~package_name in
      (
        match document_opt with
        | None -> Ok (Pubgrub.Provider.Unavailable ("package '" ^ package_name ^ "' was not found"))
        | Some document -> (
            match
              List.find_opt
                (fun (release: Pkgs_ml.Sparse_index.release) ->
                  String.equal release.version version_string)
                document.releases
            with
            | None ->
                Ok (Pubgrub.Provider.Unavailable ("package '"
                ^ package_name
                ^ "@"
                ^ version_string
                ^ "' is unavailable"))
            | Some release when release.yanked ->
                Ok (Pubgrub.Provider.Unavailable ("package '"
                ^ package_name
                ^ "@"
                ^ version_string
                ^ "' was yanked"))
            | Some release ->
                let rec loop acc = function
                  | [] -> Ok (Pubgrub.Provider.Available (List.rev acc))
                  | (dep: Pkgs_ml.Sparse_index.dependency) :: rest ->
                      if Riot_model.Package.is_builtin_dependency_name dep.name then
                        loop acc rest
                      else
                        (
                          let () = record_required_by
                            catalog
                            ~package_name:dep.name
                            (Some Riot_model.Pm_error.{ package = package_name; path = None }) in
                          let ranges =
                            match HashMap.get catalog.local_by_name dep.name with
                            | Some _ -> Pubgrub.full
                            | None -> Pubgrub.full
                          in
                          loop ((dep.name, ranges) :: acc) rest
                        )
                in
                loop [] release.dependencies
          )
      )

let provider_dependencies_of_local_entry = fun (catalog: catalog) (entry: local_entry) ->
  provider_dependencies_of_manifest
    catalog
    ~declared_from:entry.package.path
    ~required_by:(Some (required_by_local_package entry.package))
    (Riot_model.Package.all_dependencies entry.package)
  |> Result.map (fun deps -> Pubgrub.Provider.Available deps)

let provider_dependencies_of_root = fun (catalog: catalog) (
  workspace_packages: Riot_model.Package.t list
) ->
  let workspace_deps =
    List.map (fun (pkg: Riot_model.Package.t) -> (pkg.name, Pubgrub.full)) workspace_packages
  in
  let* runtime_deps = provider_dependencies_of_manifest
    catalog
    ~declared_from:catalog.ctx.workspace.root
    ~required_by:None catalog.ctx.workspace.dependencies in
  let* build_deps = provider_dependencies_of_manifest
    catalog
    ~declared_from:catalog.ctx.workspace.root
    ~required_by:None catalog.ctx.workspace.build_dependencies in
  let* dev_deps = provider_dependencies_of_manifest
    catalog
    ~declared_from:catalog.ctx.workspace.root
    ~required_by:None catalog.ctx.workspace.dev_dependencies in
  Ok (Pubgrub.Provider.Available (workspace_deps @ runtime_deps @ build_deps @ dev_deps))

let provider_of_catalog = fun (catalog: catalog) workspace_packages ->
  {
    Pubgrub.Provider.choose_version =
      (fun package ranges ->
        (
          if String.equal package pubgrub_root_package then
            Ok (Some pubgrub_root_version)
          else
            match HashMap.get catalog.local_by_name package with
            | Some entry ->
                let version = local_solver_version entry.package in
                if Pubgrub.Ranges.contains ~compare_v:pubgrub_version_compare ranges version then
                  Ok (Some version)
                else
                  Ok None
            | None ->
                let* versions = matching_registry_versions catalog ~package_name:package ~ranges in
                if List.length versions = 0 then
                  Ok None
                else
                  match catalog.ctx.mode with
                  | Refresh ->
                      let registry_name = Pkgs_ml.Registry.name catalog.ctx.registry in
                      (
                        match find_existing_registry_package
                          ~registry_name
                          ~existing_lock:catalog.ctx.existing_lock
                          ~package_name:package with
                        | Some existing_pkg -> (
                            match existing_pkg.id.version with
                            | Some version_string ->
                                let* existing_version = parse_registry_version
                                  ~package_name:package
                                  version_string in
                                if
                                  Pubgrub.Ranges.contains ~compare_v:pubgrub_version_compare ranges existing_version
                                then
                                  Ok (Some existing_version)
                                else
                                  Ok (Some (highest_version versions))
                            | None -> Ok (Some (highest_version versions))
                          )
                        | None -> Ok (Some (highest_version versions))
                      )
                  | Unlock -> Ok (Some (highest_version versions))
        ) |> Result.map_error Error.message);
    count_versions =
      (fun package ranges ->
        (
          if String.equal package pubgrub_root_package then
            Ok 1
          else
            match HashMap.get catalog.local_by_name package with
            | Some entry ->
                let version = local_solver_version entry.package in
                if Pubgrub.Ranges.contains ~compare_v:pubgrub_version_compare ranges version then
                  Ok 1
                else
                  Ok 0
            | None ->
                let* versions = matching_registry_versions catalog ~package_name:package ~ranges in
                Ok (List.length versions)
        ) |> Result.map_error Error.message);
    get_dependencies =
      (fun package version ->
        if String.equal package pubgrub_root_package then
          Result.map_error Error.message (provider_dependencies_of_root catalog workspace_packages)
        else
          match HashMap.get catalog.local_by_name package with
          | Some entry -> Result.map_error
            Error.message
            (provider_dependencies_of_local_entry catalog entry)
          | None -> Result.map_error
            Error.message
            (provider_dependencies_of_registry_package catalog ~package_name:package version));
  }

let selected_versions_of_solution = fun solution ->
  let selected = HashMap.create () in
  List.iter
    (fun (package_name, version) ->
      if not (String.equal package_name pubgrub_root_package) then
        ignore (HashMap.insert selected package_name version))
    solution;
  selected

let registry_package_id_of_solution = fun (catalog: catalog) ~package_name version ->
  let version_string = Std.Version.to_string version in
  let registry_name = Pkgs_ml.Registry.name catalog.ctx.registry in
  match find_existing_registry_package_version
    ~registry_name
    ~existing_lock:catalog.ctx.existing_lock
    ~package_name
    ~version:version_string with
  | Some pkg -> Ok pkg.id
  | None ->
      let* document_opt = read_registry_document catalog ~package_name in
      (
        match document_opt with
        | None -> Error (Error.Unexpected {
          error = "selected registry package '" ^ package_name ^ "@" ^ version_string ^ "' is unavailable"
        })
        | Some document -> (
            match
              List.find_opt
                (fun (release: Pkgs_ml.Sparse_index.release) ->
                  String.equal release.version version_string)
                document.releases
            with
            | Some release -> Ok Riot_model.Lockfile.{
              registry = Some registry_name;
              name = package_name;
              version = Some version_string;
              sha256 = Some release.artifact_sha256
            }
            | None -> Error (Error.Unexpected {
              error = "selected registry package '" ^ package_name ^ "@" ^ version_string ^ "' is missing from the sparse index"
            })
          )
      )

let selected_package_id = fun (catalog: catalog) ~selected_versions ~package_name ->
  match HashMap.get catalog.local_by_name package_name with
  | Some entry -> Ok (package_id_of_local_entry entry)
  | None -> (
      match HashMap.get selected_versions package_name with
      | Some version -> registry_package_id_of_solution catalog ~package_name version
      | None -> Error (Error.Unexpected {
        error = "dependency solver did not select a package for '" ^ package_name ^ "'"
      })
    )

let lock_dependencies_of_manifest = fun (catalog: catalog) ~selected_versions ~declared_from deps ->
  let rec loop acc = function
    | [] -> Ok (List.rev acc)
    | (dep: Riot_model.Package.dependency) :: rest ->
        if dep.source.builtin then
          loop acc rest
        else
          let* target_name_opt = dependency_target_name catalog ~declared_from ~required_by:None dep in
          match target_name_opt with
          | None -> loop acc rest
          | Some target_name ->
              let* package = selected_package_id catalog ~selected_versions ~package_name:target_name in
              loop (Riot_model.Lockfile.{ name = dep.name; package } :: acc) rest
  in
  loop [] deps

let lock_package_of_local_entry = fun (catalog: catalog) ~selected_versions (entry: local_entry) ->
  let* dependencies = lock_dependencies_of_manifest
    catalog
    ~selected_versions
    ~declared_from:entry.package.path
    entry.package.dependencies in
  let* build_dependencies = lock_dependencies_of_manifest
    catalog
    ~selected_versions
    ~declared_from:entry.package.path
    entry.package.build_dependencies in
  let* dev_dependencies = lock_dependencies_of_manifest
    catalog
    ~selected_versions
    ~declared_from:entry.package.path
    entry.package.dev_dependencies in
  Ok Riot_model.Lockfile.{
    id = package_id_of_local_entry entry;
    root = lock_root_of_local_entry ~workspace_root:catalog.ctx.workspace.root entry;
    provenance = entry.provenance;
    dependencies;
    build_dependencies;
    dev_dependencies;
  }

let pm_error_of_pubgrub_failure = fun (catalog: catalog) incompat ->
  let registry_name = Pkgs_ml.Registry.name catalog.ctx.registry in
  let rec find_no_versions = function
    | Pubgrub.Incompatibility.External {
      cause=Pubgrub.Incompatibility.NoVersions (package_name, _);
      _;

    } ->
        if
          String.equal package_name pubgrub_root_package
          || match HashMap.get catalog.local_by_name package_name with
          | Some _ -> true
          | None -> false
        then
          None
        else
          Some package_name
    | Pubgrub.Incompatibility.Derived { cause1; cause2; _ } -> (
        match find_no_versions cause1 with
        | Some _ as found -> found
        | None -> find_no_versions cause2
      )
    | _ ->
        None
  in
  match find_no_versions incompat with
  | Some package_name -> (
      match HashMap.get catalog.registry_documents package_name with
      | Some None ->
          Error.PackageNotFound {
            package = package_name;
            registry = registry_name;
            required_by = required_by_for_package catalog package_name
          }
      | Some (Some document) ->
          let available_versions = document.releases
          |> List.map (fun (release: Pkgs_ml.Sparse_index.release) -> release.version)
          |> sort_registry_versions ~package_name in
          let requirement =
            match requested_requirement_for_package catalog package_name with
            | Some requirement -> requirement
            | None -> "the requested range"
          in
          Error.RegistryVersionNotFound {
            package = package_name;
            registry = registry_name;
            requirement;
            available_versions;
            required_by = required_by_for_package catalog package_name;
          }
      | None ->
          Error.PackageNotFound {
            package = package_name;
            registry = registry_name;
            required_by = required_by_for_package catalog package_name
          }
    )
  | _ -> Error.Unexpected { error = Pubgrub.explain_conflict incompat }

let lock_registry_package = fun (catalog: catalog) ~selected_versions ~package_name version ->
  let registry_name = Pkgs_ml.Registry.name catalog.ctx.registry in
  let version_string = Std.Version.to_string version in
  match find_existing_registry_package_version
    ~registry_name
    ~existing_lock:catalog.ctx.existing_lock
    ~package_name
    ~version:version_string with
  | Some pkg when catalog.ctx.mode = Refresh -> Ok pkg
  | _ ->
      let* package_id = registry_package_id_of_solution catalog ~package_name version in
      let* dependencies_result = provider_dependencies_of_registry_package catalog ~package_name version in
      let dependency_specs =
        match dependencies_result with
        | Pubgrub.Provider.Available dependency_specs -> dependency_specs
        | Pubgrub.Provider.Unavailable reason -> []
      in
      let rec loop acc = function
        | [] -> Ok (List.rev acc)
        | (dependency_name, _ranges) :: rest ->
            let* package = selected_package_id catalog ~selected_versions ~package_name:dependency_name in
            loop (Riot_model.Lockfile.{ name = dependency_name; package } :: acc) rest
      in
      let* dependencies = loop [] dependency_specs in
      Ok Riot_model.Lockfile.{
        id = package_id;
        root = None;
        provenance = Riot_model.Lockfile.Registry { registry = registry_name };
        dependencies;
        build_dependencies = [];
        dev_dependencies = [];
      }

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
  let workspace_packages = List.filter Riot_model.Package.is_workspace_member workspace.packages in
  let catalog = create_catalog ~ctx in
  let* () = register_workspace_packages catalog workspace_packages in
  emit
    (Riot_model.Event.DependencyUniverseBuilding {
      packages = List.map (fun (pkg: Riot_model.Package.t) -> pkg.name) workspace_packages
    });
  let provider = provider_of_catalog catalog workspace_packages in
  match Pubgrub.solve provider pubgrub_root_package pubgrub_root_version with
  | Error message ->
      Error (Error.Unexpected { error = message })
  | Ok (Pubgrub.Solver.Failure incompat) ->
      Error (pm_error_of_pubgrub_failure catalog incompat)
  | Ok (Pubgrub.Solver.Success solution) ->
      let selected_versions = selected_versions_of_solution solution in
      let rec lock_workspace_packages acc = function
        | [] -> Ok (List.rev acc)
        | (pkg: Riot_model.Package.t) :: rest ->
            let* lock_package = lock_package_of_local_entry
              catalog
              ~selected_versions
              { package = pkg; provenance = Riot_model.Lockfile.Workspace } in
            lock_workspace_packages (lock_package :: acc) rest
      in
      let selected_external_local_names =
        List.filter
          (fun package_name ->
            not
              (
                List.exists
                  (fun (pkg: Riot_model.Package.t) ->
                    String.equal pkg.name package_name)
                  workspace_packages
              ) && not (String.equal package_name pubgrub_root_package) && match HashMap.get
              selected_versions
              package_name with
            | Some _ -> true
            | None -> false)
          catalog.local_order
      in
      let rec lock_external_local_packages acc = function
        | [] -> Ok (List.rev acc)
        | package_name :: rest -> (
            match HashMap.get catalog.local_by_name package_name with
            | Some entry ->
                let* lock_package = lock_package_of_local_entry catalog ~selected_versions entry in
                lock_external_local_packages (lock_package :: acc) rest
            | None -> lock_external_local_packages acc rest
          )
      in
      let selected_registry_names =
        let names = ref [] in
        HashMap.iter
          (fun package_name _version ->
            if
              not (String.equal package_name pubgrub_root_package)
              && match HashMap.get catalog.local_by_name package_name with
              | Some _ -> false
              | None -> true
            then
              names := package_name :: !names)
          selected_versions;
        List.rev !names
      in
      let rec lock_registry_packages acc = function
        | [] -> Ok (List.rev acc)
        | package_name :: rest -> (
            match HashMap.get selected_versions package_name with
            | Some version ->
                let* lock_package = lock_registry_package catalog ~selected_versions ~package_name version in
                lock_registry_packages (lock_package :: acc) rest
            | None -> lock_registry_packages acc rest
          )
      in
      let* workspace_lock_packages = lock_workspace_packages [] workspace_packages in
      let* external_local_packages = lock_external_local_packages [] selected_external_local_names in
      let* registry_packages = lock_registry_packages [] selected_registry_names in
      let preserved =
        match (mode, existing_lock) with
        | Unlock, _ -> []
        | Refresh, Some (existing_lock: Riot_model.Lockfile.t) -> existing_lock.packages
        |> List.filter (keep_existing_package workspace_packages)
        | Refresh, None -> []
      in
      let packages = merge_lock_packages
        (workspace_lock_packages @ external_local_packages @ registry_packages @ preserved) in
      let runtime_packages, build_packages, dev_packages = dependency_counts packages in
      emit
        (Riot_model.Event.DependencyUniverseBuilt {
          runtime_packages;
          build_packages;
          dev_packages;
          duration_ms = duration_ms_since started
        });
      Ok Riot_model.Lockfile.{ format_version = 1; dependency_hash = ""; packages }
