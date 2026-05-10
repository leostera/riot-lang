open Std
open Std.Result.Syntax

module Deps_error = Error

open Riot_model

let duration_ms_since = fun started ->
  Time.Instant.duration_since ~earlier:started (Time.Instant.now ())
  |> Time.Duration.to_millis

type dependency_scope =
  | Runtime
  | Build
  | Dev

type manifest_selection =
  | Current
  | Workspace
  | Package of Package_name.t

type suggested_package = {
  package: string;
  latest_version: string;
  description: string option;
}

type search_request = { query: string; limit: int }

type loaded_workspace = {
  workspace: Workspace.t;
  package_name: Package_name.t;
}

type event_sink = Event.deps_event -> unit

type add_request = {
  selection: manifest_selection;
  scope: dependency_scope;
  dependencies: string list;
}

type remove_request = {
  selection: manifest_selection;
  scope: dependency_scope;
  dependencies: Package_name.t list;
}

type update_request = {
  packages: Package_name.t list;
}

type dependency_spec_error =
  | RegistryDependencySpecError of Registry_package_spec.error
  | SourceDependencySpecError of Git_dependency.error

type path_dependency_load_error =
  | PathDependencyManifestReadFailed of IO.error
  | PathDependencyTomlParseFailed of Std.Data.Toml.error
  | PathDependencyManifestDecodeFailed of Package.manifest_error

type source_dependency_load_error =
  | SourceDependencyMaterializationFailed of Git_dependency.error
  | SourceDependencyManifestReadFailed of IO.error
  | SourceDependencyTomlParseFailed of Std.Data.Toml.error
  | SourceDependencyManifestDecodeFailed of Package.manifest_error

type registry_initialization_error =
  | RegistryFilesystemInitializationFailed of Pkgs_ml.Registry_cache.create_error

type registry_lookup_error =
  | RegistryPackageDocumentReadFailed of string
  | RegistryPackageNameDecodeFailed of Package_name.error

type registry_search_error =
  | RegistrySearchRequestFailed of string

type registry_materialization_error =
  | RegistryPackageMaterializationFailed of Deps_error.t
  | RegistryPackageManifestReadFailed of IO.error
  | RegistryPackageTomlParseFailed of Std.Data.Toml.error
  | RegistryPackageManifestDecodeFailed of Package.manifest_error

type error =
  | CurrentPackageNotFound of {
      cwd: Path.t;
    }
  | PackageNotFound of {
      package: Package_name.t;
    }
  | DependencySpecInvalid of {
      dependency: string;
      error: dependency_spec_error;
    }
  | PathDependencyMustBeRelative of { dependency: string }
  | PathDependencyLoadFailed of {
      dependency: string;
      path: Path.t;
      error: path_dependency_load_error;
    }
  | SourceDependencyLoadFailed of {
      dependency: string;
      source_locator: string;
      ref_: string option;
      error: source_dependency_load_error;
    }
  | RegistryInitializationFailed of {
      registry: string;
      error: registry_initialization_error;
    }
  | RegistryLookupFailed of {
      package: string;
      registry: string;
      error: registry_lookup_error;
    }
  | RegistryMaterializationFailed of {
      package: string;
      version: string;
      registry: string;
      error: registry_materialization_error;
    }
  | RegistrySearchFailed of {
      query: string;
      registry: string;
      error: registry_search_error;
    }
  | RegistryPackageNotFound of {
      package: string;
      registry: string;
      suggestions: suggested_package list;
    }
  | RegistryReleaseYanked of { package: string; version: string; registry: string }
  | RegistryVersionNotFound of { package: string; requirement: string; registry: string }
  | ManifestUpdateFailed of Manifest_edit.error
  | DependencyNotFoundInSection of {
      path: Path.t;
      section: string;
      dependency: string;
    }
  | WorkspaceReloadFailed of {
      workspace_root: Path.t;
      error: Workspace_manager.scan_error;
    }
  | WorkspaceReloadHadErrors of {
      workspace_root: Path.t;
      errors: Workspace_manager.load_error list;
    }
  | MaterializedPackageNotFound of {
      package_root: Path.t;
      workspace_root: Path.t;
    }
  | LockRefreshFailed of Deps_error.t

type target_manifest = {
  path: Path.t;
  dependencies: Package.dependency list;
}

type registry_dependency = Registry_package_spec.t = {
  name: Package_name.t;
  requirement: Std.Version.requirement option;
}

type path_dependency = {
  name: Package_name.t;
  path: Path.t;
}

type source_dependency = {
  name: Package_name.t;
  source_locator: string;
  ref_: string option;
}

type parsed_dependency =
  | Registry of registry_dependency
  | Path of path_dependency
  | Source of source_dependency

let dependency_spec_error_message = fun __tmp1 ->
  match __tmp1 with
  | RegistryDependencySpecError error -> Registry_package_spec.error_message error
  | SourceDependencySpecError error -> Git_dependency.message error

let path_dependency_load_error_message = fun __tmp1 ->
  match __tmp1 with
  | PathDependencyManifestReadFailed error -> IO.error_message error
  | PathDependencyTomlParseFailed error -> Data.Toml.error_to_string error
  | PathDependencyManifestDecodeFailed error -> Package.manifest_error_message error

let source_dependency_load_error_message = fun __tmp1 ->
  match __tmp1 with
  | SourceDependencyMaterializationFailed error -> Git_dependency.message error
  | SourceDependencyManifestReadFailed error -> IO.error_message error
  | SourceDependencyTomlParseFailed error -> Data.Toml.error_to_string error
  | SourceDependencyManifestDecodeFailed error -> Package.manifest_error_message error

let registry_initialization_error_message = fun (RegistryFilesystemInitializationFailed error) ->
  Pkgs_ml.Registry_cache.create_error_message
    error

let registry_lookup_error_message = fun __tmp1 ->
  match __tmp1 with
  | RegistryPackageDocumentReadFailed error -> error
  | RegistryPackageNameDecodeFailed error -> Package_name.error_message error

let registry_search_error_message = fun (RegistrySearchRequestFailed error) -> error

let registry_materialization_error_message = fun __tmp1 ->
  match __tmp1 with
  | RegistryPackageMaterializationFailed error -> Deps_error.message error
  | RegistryPackageManifestReadFailed error -> IO.error_message error
  | RegistryPackageTomlParseFailed error -> Data.Toml.error_to_string error
  | RegistryPackageManifestDecodeFailed error -> Package.manifest_error_message error

let no_emit: event_sink = fun _ -> ()

let registry_name = "pkgs.ml"

let registry_dependency_name = fun (dep: registry_dependency) -> dep.name

let should_emit_source_materialization_started = fun ~source_locator ~update ->
  if update then
    true
  else
    match Git_dependency.parse_source_locator source_locator with
    | Error _ -> true
    | Ok locator -> (
        let repo_dir =
          Riot_model.Riot_dirs.git_registry_repo_dir
            ~host:locator.host
            ~owner:locator.owner
            ~repo:locator.repo
        in
        match Fs.exists repo_dir with
        | Ok exists -> not exists
        | Error _ -> true
      )

let error_message = fun __tmp1 ->
  match __tmp1 with
  | CurrentPackageNotFound { cwd } ->
      "could not determine current package from '" ^ Path.to_string cwd ^ "'"
  | PackageNotFound { package } ->
      "workspace package '" ^ Package_name.to_string package ^ "' was not found"
  | DependencySpecInvalid { dependency; error } ->
      "invalid dependency '" ^ dependency ^ "': " ^ dependency_spec_error_message error
  | PathDependencyMustBeRelative { dependency } ->
      "path dependency '" ^ dependency ^ "' must be a relative path"
  | PathDependencyLoadFailed { dependency; path; error } ->
      "failed to load path dependency '"
      ^ dependency
      ^ "' from '"
      ^ Path.to_string path
      ^ "': "
      ^ path_dependency_load_error_message error
  | SourceDependencyLoadFailed {
      dependency;
      source_locator;
      ref_;
      error;
    } ->
      let suffix =
        match ref_ with
        | Some ref_ -> "#" ^ ref_
        | None -> ""
      in
      "failed to load source dependency '"
      ^ dependency
      ^ "' from '"
      ^ source_locator
      ^ suffix
      ^ "': "
      ^ source_dependency_load_error_message error
  | RegistryInitializationFailed { registry; error } ->
      "failed to initialize registry '"
      ^ registry
      ^ "': "
      ^ registry_initialization_error_message error
  | RegistryLookupFailed { package; registry; error } ->
      "failed to look up package '"
      ^ package
      ^ "' in registry '"
      ^ registry
      ^ "': "
      ^ registry_lookup_error_message error
  | RegistryMaterializationFailed {
      package;
      version;
      registry;
      error;
    } ->
      "failed to materialize package '"
      ^ package
      ^ "@"
      ^ version
      ^ "' from registry '"
      ^ registry
      ^ "': "
      ^ registry_materialization_error_message error
  | RegistrySearchFailed { query; registry; error } ->
      "failed to search registry '"
      ^ registry
      ^ "' for '"
      ^ query
      ^ "': "
      ^ registry_search_error_message error
  | RegistryPackageNotFound { package; registry; suggestions } ->
      let base = "package '" ^ package ^ "' was not found in registry '" ^ registry ^ "'" in
      (
        match suggestions with
        | [] -> base
        | suggestions ->
            let lines =
              List.map
                suggestions
                ~fn:(fun { package; latest_version; description } ->
                  match description with
                  | Some description ->
                      "  - " ^ package ^ "@" ^ latest_version ^ " - " ^ description
                  | None -> "  - " ^ package ^ "@" ^ latest_version)
            in
            base ^ "\nDid you mean:\n" ^ String.concat "\n" lines
      )
  | RegistryReleaseYanked { package; version; registry } ->
      "package '" ^ package ^ "@" ^ version ^ "' was yanked from registry '" ^ registry ^ "'"
  | RegistryVersionNotFound { package; requirement; registry } ->
      "package '"
      ^ package
      ^ "' has no release matching '"
      ^ requirement
      ^ "' in registry '"
      ^ registry
      ^ "'"
  | ManifestUpdateFailed error -> Manifest_edit.error_message error
  | DependencyNotFoundInSection { path; section; dependency } ->
      "dependency '"
      ^ dependency
      ^ "' was not found in ["
      ^ section
      ^ "] of '"
      ^ Path.to_string path
      ^ "'"
  | WorkspaceReloadFailed { workspace_root; error } ->
      "failed to reload workspace '"
      ^ Path.to_string workspace_root
      ^ "': "
      ^ Workspace_manager.scan_error_message error
  | WorkspaceReloadHadErrors { workspace_root; errors } ->
      "workspace '"
      ^ Path.to_string workspace_root
      ^ "' has load errors:\n"
      ^ String.concat "\n" (List.map errors ~fn:Workspace_manager.load_error_to_string)
  | MaterializedPackageNotFound { package_root; workspace_root } ->
      "materialized package root '"
      ^ Path.to_string package_root
      ^ "' does not correspond to a package in workspace '"
      ^ Path.to_string workspace_root
      ^ "'"
  | LockRefreshFailed error -> Deps_error.message error

let scope_to_section = fun __tmp1 ->
  match __tmp1 with
  | Runtime -> Manifest_edit.Runtime
  | Build -> Manifest_edit.Build
  | Dev -> Manifest_edit.Dev

let parse_registry_dependency_spec = fun raw ->
  Registry_package_spec.from_string raw
  |> Result.map_err
    ~fn:(fun error ->
      DependencySpecInvalid { dependency = raw; error = RegistryDependencySpecError error })

let is_source_dependency_spec = fun raw ->
  String.starts_with ~prefix:"http://" raw
  || String.starts_with ~prefix:"https://" raw
  || String.starts_with ~prefix:"github.com/" raw

let dependency_root = fun ~declared_from dep_path ->
  if Path.is_absolute dep_path then
    Path.normalize dep_path
  else
    Path.normalize Path.(declared_from / dep_path)

let load_path_dependency = fun ~(target:target_manifest) ~raw ->
  let dep_path = Path.normalize (Path.v raw) in
  if Path.is_absolute dep_path then
    Error (PathDependencyMustBeRelative { dependency = raw })
  else
    let declared_from =
      match Path.parent target.path with
      | Some parent -> parent
      | None -> Path.v "."
    in
    let package_root = dependency_root ~declared_from dep_path in
    let manifest_path = Path.(package_root / Path.v "riot.toml") in
    let* source =
      Fs.read_to_string manifest_path
      |> Result.map_err
        ~fn:(fun err ->
          PathDependencyLoadFailed {
            dependency = raw;
            path = package_root;
            error = PathDependencyManifestReadFailed err;
          })
    in
    let* toml =
      Data.Toml.parse source
      |> Result.map_err
        ~fn:(fun err ->
          PathDependencyLoadFailed {
            dependency = raw;
            path = package_root;
            error = PathDependencyTomlParseFailed err;
          })
    in
    let* package =
      Package.from_toml
        toml
        ~workspace_deps:[]
        ~workspace_dev_deps:[]
        ~workspace_build_deps:[]
        ~path:package_root
        ~relative_path:dep_path
      |> Result.map_err
        ~fn:(fun err ->
          PathDependencyLoadFailed {
            dependency = raw;
            path = package_root;
            error = PathDependencyManifestDecodeFailed err;
          })
    in
    Ok (Path { name = package.name; path = dep_path })

let load_source_dependency = fun ~(emit:event_sink) ~raw ->
  let* spec =
    Git_dependency.parse_spec raw
    |> Result.map_err
      ~fn:(fun error ->
        DependencySpecInvalid { dependency = raw; error = SourceDependencySpecError error })
  in
  let* () =
    Git_dependency.parse_source_locator spec.source_locator
    |> Result.map_err
      ~fn:(fun error ->
        DependencySpecInvalid { dependency = raw; error = SourceDependencySpecError error })
    |> Result.map ~fn:(fun _ -> ())
  in
  let emit_materialization_events =
    should_emit_source_materialization_started ~source_locator:spec.source_locator ~update:true
  in
  if emit_materialization_events then
    emit
      (Riot_model.Event.DepsSourceMaterializationStarted {
        source_locator = spec.source_locator;
        ref_ = spec.ref_;
      });
  let* materialized =
    Git_dependency.materialize ~source_locator:spec.source_locator ~ref_:spec.ref_ ()
    |> Result.map_err
      ~fn:(fun error ->
        SourceDependencyLoadFailed {
          dependency = raw;
          source_locator = spec.source_locator;
          ref_ = spec.ref_;
          error = SourceDependencyMaterializationFailed error;
        })
  in
  let manifest_path = Path.(materialized.package_root / Path.v "riot.toml") in
  let* source =
    Fs.read_to_string manifest_path
    |> Result.map_err
      ~fn:(fun err ->
        SourceDependencyLoadFailed {
          dependency = raw;
          source_locator = spec.source_locator;
          ref_ = spec.ref_;
          error = SourceDependencyManifestReadFailed err;
        })
  in
  let* toml =
    Data.Toml.parse source
    |> Result.map_err
      ~fn:(fun err ->
        SourceDependencyLoadFailed {
          dependency = raw;
          source_locator = spec.source_locator;
          ref_ = spec.ref_;
          error = SourceDependencyTomlParseFailed err;
        })
  in
  let relative_path =
    match Path.strip_prefix materialized.package_root ~prefix:materialized.repository_root with
    | Ok relative_path -> relative_path
    | Error _ -> Path.v "."
  in
  let* package =
    Package_manifest.from_toml
      toml
      ~workspace_deps:[]
      ~workspace_dev_deps:[]
      ~workspace_build_deps:[]
      ~path:materialized.package_root
      ~relative_path
    |> Result.map_err
      ~fn:(fun error ->
        SourceDependencyLoadFailed {
          dependency = raw;
          source_locator = spec.source_locator;
          ref_ = spec.ref_;
          error = SourceDependencyManifestDecodeFailed error;
        })
  in
  if emit_materialization_events then
    emit
      (
        Riot_model.Event.DepsSourceMaterializationFinished {
          source_locator = spec.source_locator;
          ref_ = spec.ref_;
          package = package.name;
          version = Option.map package.publish.version ~fn:Std.Version.to_string;
        }
      );
  Ok (Source { name = package.name; source_locator = spec.source_locator; ref_ = spec.ref_ })

let parse_dependency_spec = fun ~(target:target_manifest) raw ->
  if is_source_dependency_spec raw then
    load_source_dependency ~emit:no_emit ~raw
  else
    match parse_registry_dependency_spec raw with
    | Ok parsed -> Ok (Registry parsed)
    | Error _ when String.contains raw "/" || String.starts_with ~prefix:"." raw ->
        load_path_dependency ~target ~raw
    | Error err -> Error err

let init_registry = fun () ->
  Pkgs_ml.Registry.create_filesystem ~registry_name ()
  |> Result.map_err
    ~fn:(fun error ->
      RegistryInitializationFailed {
        registry = registry_name;
        error = RegistryFilesystemInitializationFailed error;
      })

let ensure_loaded_workspace = fun
  ~workspace_manager ~registry ~(workspace:Workspace_manifest.t) ~package_name () ->
  let* workspace =
    Workspace_resolution.ensure_workspace
      ~workspace_manager
      ~mode:Dep_solver.Refresh
      ~registry
      ~workspace
      ()
    |> Result.map_err ~fn:(fun error -> LockRefreshFailed error)
  in
  Ok { workspace; package_name }

let select_materialized_package = fun
  ~(workspace:Riot_model.Workspace_manifest.t) ?preferred_package_name ~package_root () ->
  match Riot_model.Workspace_manifest.find_package_for_path workspace ~path:package_root with
  | Some pkg -> Ok pkg.name
  | None -> (
      match preferred_package_name with
      | Some preferred_package_name -> (
          match List.find
            workspace.packages
            ~fn:(fun (pkg: Riot_model.Package_manifest.t) ->
              Riot_model.Package_name.equal
                pkg.name
                preferred_package_name) with
          | Some pkg -> Ok pkg.name
          | None ->
              match workspace.packages with
              | [ pkg ] -> Ok pkg.name
              | _ -> (
                  match List.filter
                    workspace.packages
                    ~fn:Riot_model.Package_manifest.is_workspace_member with
                  | [ pkg ] -> Ok pkg.name
                  | _ ->
                      Error (MaterializedPackageNotFound {
                        package_root;
                        workspace_root = workspace.root;
                      })
                )
        )
      | None ->
          match workspace.packages with
          | [ pkg ] -> Ok pkg.name
          | _ -> (
              match List.filter
                workspace.packages
                ~fn:Riot_model.Package_manifest.is_workspace_member with
              | [ pkg ] -> Ok pkg.name
              | _ ->
                  Error (MaterializedPackageNotFound {
                    package_root;
                    workspace_root = workspace.root;
                  })
            )
    )

let scan_workspace_from_root = fun ~workspace_manager ~package_root () ->
  let* (workspace, load_errors) =
    Riot_model.Workspace_manager.scan workspace_manager package_root
    |> Result.map_err
      ~fn:(fun error -> WorkspaceReloadFailed { workspace_root = package_root; error })
  in
  match load_errors with
  | [] -> Ok workspace
  | load_errors ->
      Error (WorkspaceReloadHadErrors { workspace_root = workspace.root; errors = load_errors })

let matching_release_of_document = fun
  (document: Pkgs_ml.Sparse_index.package_document) requirement ->
  let matches =
    List.filter_map
      document.releases
      ~fn:(fun (release: Pkgs_ml.Sparse_index.release) ->
        match Std.Version.parse release.version with
        | Ok _ when release.yanked -> None
        | Ok version when Std.Version.matches requirement version -> Some (version, release)
        | Ok _
        | Error _ -> None)
  in
  match List.sort
    matches
    ~compare:(fun (left, _) (right, _) ->
      match Std.Version.compare left right with
      | Order.LT -> Order.GT
      | Order.EQ -> Order.EQ
      | Order.GT -> Order.LT) with
  | (_, release) :: _ -> Some release
  | [] -> None

let decode_detached_package = fun ~package_root ->
  let manifest_path = Path.(package_root / Path.v "riot.toml") in
  let* source =
    Fs.read_to_string manifest_path
    |> Result.map_err ~fn:(fun err -> IO.error_message err)
  in
  let* toml =
    Data.Toml.parse source
    |> Result.map_err ~fn:Data.Toml.error_to_string
  in
  Riot_model.Package_manifest.from_toml
    toml
    ~workspace_deps:[]
    ~workspace_dev_deps:[]
    ~workspace_build_deps:[]
    ~path:package_root
    ~relative_path:(Path.v ".")
  |> Result.map_err ~fn:Riot_model.Package_manifest.error_message

let dependency_lists_for_package = fun scope (pkg: Riot_model.Package_manifest.t) ->
  match scope with
  | Runtime -> pkg.dependencies
  | Build -> pkg.build_dependencies
  | Dev -> pkg.dev_dependencies

let dependency_lists_for_workspace = fun scope (workspace: Riot_model.Workspace_manifest.t) ->
  match scope with
  | Runtime -> workspace.dependencies
  | Build -> workspace.build_dependencies
  | Dev -> workspace.dev_dependencies

let dependency_names_for_section = fun ~manifest_path ~section ->
  let section_name = Manifest_edit.section_name (scope_to_section section) in
  let* source =
    Fs.read_to_string manifest_path
    |> Result.map_err
      ~fn:(fun err ->
        ManifestUpdateFailed (Manifest_edit.ReadFailed { path = manifest_path; error = err }))
  in
  let* toml =
    Data.Toml.parse source
    |> Result.map_err
      ~fn:(fun err ->
        ManifestUpdateFailed (Manifest_edit.TomlParseFailed { path = manifest_path; error = err }))
  in
  match toml with
  | Data.Toml.Table fields -> (
      match List.find fields ~fn:(fun (name, _) -> String.equal name section_name)
      |> Option.map ~fn:(fun (_, value) -> value) with
      | None -> Ok []
      | Some (Data.Toml.Table dep_items) ->
          let rec loop acc = fun __tmp1 ->
            match __tmp1 with
            | [] -> Ok (List.reverse acc)
            | (name, _) :: rest ->
                let* name =
                  Riot_model.Package_name.from_string name
                  |> Result.map_err
                    ~fn:(fun error ->
                      ManifestUpdateFailed (Manifest_edit.InvalidDependencyName {
                        path = manifest_path;
                        dependency = name;
                        error;
                      }))
                in
                loop (name :: acc) rest
          in
          loop [] dep_items
      | Some _ ->
          Error (ManifestUpdateFailed (Manifest_edit.DependencySectionMustBeTable {
            path = manifest_path;
            section = section_name;
          }))
    )
  | _ -> Error (ManifestUpdateFailed (Manifest_edit.ManifestMustBeTable { path = manifest_path }))

let filter_dependencies_by_names = fun names dependencies ->
  List.filter
    dependencies
    ~fn:(fun (dep: Riot_model.Package.dependency) -> List.contains names ~value:dep.name)

let select_current_package = fun ~(workspace:Riot_model.Workspace_manifest.t) ~cwd ->
  match Riot_model.Workspace_manifest.find_package_for_path workspace ~path:cwd with
  | Some pkg -> Ok pkg
  | None -> (
      match List.filter workspace.packages ~fn:Riot_model.Package_manifest.is_workspace_member with
      | [ pkg ] -> Ok pkg
      | _ -> Error (CurrentPackageNotFound { cwd })
    )

let target_manifest = fun ~(workspace:Riot_model.Workspace_manifest.t) ~cwd selection scope ->
  match selection with
  | Workspace ->
      let path = Path.(workspace.root / Path.v "riot.toml") in
      let* names = dependency_names_for_section ~manifest_path:path ~section:scope in
      Ok {
        path;
        dependencies = filter_dependencies_by_names
          names
          (dependency_lists_for_workspace scope workspace);
      }
  | Package package_name -> (
      match workspace.packages
      |> List.filter ~fn:Riot_model.Package_manifest.is_workspace_member
      |> List.find
        ~fn:(fun (pkg: Riot_model.Package_manifest.t) -> Package_name.equal pkg.name package_name) with
      | Some pkg ->
          let path = Path.(pkg.path / Path.v "riot.toml") in
          let* names = dependency_names_for_section ~manifest_path:path ~section:scope in
          Ok {
            path;
            dependencies = filter_dependencies_by_names
              names
              (dependency_lists_for_package scope pkg);
          }
      | None -> Error (PackageNotFound { package = package_name })
    )
  | Current ->
      let* pkg = select_current_package ~workspace ~cwd in
      let path = Path.(pkg.path / Path.v "riot.toml") in
      let* names = dependency_names_for_section ~manifest_path:path ~section:scope in
      Ok {
        path;
        dependencies = filter_dependencies_by_names names (dependency_lists_for_package scope pkg);
      }

let dependency_exists = fun ~(package_name:string) document requirement ->
  let rec loop = fun __tmp1 ->
    match __tmp1 with
    | [] -> false
    | release :: rest -> (
        if release.Pkgs_ml.Sparse_index.yanked then
          loop rest
        else
          match Std.Version.parse release.Pkgs_ml.Sparse_index.version with
          | Ok version ->
              if Std.Version.matches requirement version then
                true
              else
                loop rest
          | Error _ -> loop rest
      )
  in
  loop document.Pkgs_ml.Sparse_index.releases

let yanked_release_of_document = fun
  (document: Pkgs_ml.Sparse_index.package_document) requirement ->
  List.find
    document.releases
    ~fn:(fun (release: Pkgs_ml.Sparse_index.release) ->
      if not release.yanked then
        false
      else
        match Std.Version.parse release.version with
        | Ok version -> Std.Version.matches requirement version
        | Error _ -> false)

let suggested_package_of_search_result = fun (result: Pkgs_ml.Registry.search_result) -> {
  package = result.package_name;
  latest_version = result.latest_version;
  description = result.description;
}

let lookup_package_suggestions = fun ~registry ~package_name ->
  match Pkgs_ml.Registry.search_packages registry ~query:package_name ~limit:5 () with
  | Ok results ->
      results
      |> List.filter
        ~fn:(fun (result: Pkgs_ml.Registry.search_result) ->
          not
            (String.equal
              (Pkgs_ml.Sparse_index.normalized_name result.package_name)
              (Pkgs_ml.Sparse_index.normalized_name package_name)))
      |> List.map ~fn:suggested_package_of_search_result
  | Error _ -> []

let search = fun ?registry ~(request:search_request) () ->
  let* registry =
    match registry with
    | Some registry -> Ok registry
    | None -> init_registry ()
  in
  let* results =
    Pkgs_ml.Registry.search_packages registry ~query:request.query ~limit:request.limit ()
    |> Result.map_err
      ~fn:(fun error ->
        RegistrySearchFailed {
          query = request.query;
          registry = Pkgs_ml.Registry.name registry;
          error = RegistrySearchRequestFailed error;
        })
  in
  Ok (List.map results ~fn:suggested_package_of_search_result)

let lookup_named_package = fun ~(emit:event_sink) ~registry (parsed: registry_dependency) ->
  let package_name = registry_dependency_name parsed in
  let package_name_string = Riot_model.Package_name.to_string package_name in
  let registry_name = Pkgs_ml.Registry.name registry in
  let started = Time.Instant.now () in
  emit
    (Riot_model.Event.DepsPackageMetadataFetchStarted {
      registry = registry_name;
      package = package_name;
    });
  let* document =
    Pkgs_ml.Registry.read_package_document registry ~package_name:package_name_string
    |> Result.map_err
      ~fn:(fun error ->
        emit
          (Riot_model.Event.DepsPackageMetadataFetchFailed {
            registry = registry_name;
            package = package_name;
            error = Riot_model.Pm_error.PackageMetadataReadFailed {
              package = package_name_string;
              registry = registry_name;
              error;
            };
          });
        RegistryLookupFailed {
          package = package_name_string;
          registry = registry_name;
          error = RegistryPackageDocumentReadFailed error;
        })
  in
  match document with
  | None ->
      emit
        (Riot_model.Event.DepsPackageMetadataFetchFailed {
          registry = registry_name;
          package = package_name;
          error = Riot_model.Pm_error.PackageNotFound {
            package = package_name_string;
            registry = registry_name;
            required_by = None;
          };
        });
      Error (RegistryPackageNotFound {
        package = package_name_string;
        registry = registry_name;
        suggestions = lookup_package_suggestions ~registry ~package_name:package_name_string;
      })
  | Some document ->
      let* document_name =
        Riot_model.Package_name.from_string document.name
        |> Result.map_err
          ~fn:(fun error ->
            RegistryLookupFailed {
              package = document.name;
              registry = registry_name;
              error = RegistryPackageNameDecodeFailed error;
            })
      in
      emit
        (
          Riot_model.Event.DepsPackageMetadataFetchFinished {
            registry = registry_name;
            package = document_name;
            version = Some document.latest;
            duration_ms = duration_ms_since started;
          }
        );
      let requirement = Option.unwrap_or ~default:Std.Version.any parsed.requirement in
      if dependency_exists ~package_name:package_name_string document requirement then
        Ok parsed
      else
        (
          match yanked_release_of_document document requirement with
          | Some release ->
              Error (RegistryReleaseYanked {
                package = package_name_string;
                version = release.version;
                registry = registry_name;
              })
          | None ->
              Error (RegistryVersionNotFound {
                package = package_name_string;
                requirement = Std.Version.requirement_to_string requirement;
                registry = registry_name;
              })
        )

let load_source_workspace_from_spec = fun
  ?(emit = no_emit) ~workspace_manager ?(update = true) ~spec () ->
  let parsed = spec in
  let source_spec = Git_dependency.to_string spec in
  let* () =
    Git_dependency.parse_source_locator parsed.source_locator
    |> Result.map_err
      ~fn:(fun error ->
        DependencySpecInvalid {
          dependency = source_spec;
          error = SourceDependencySpecError error;
        })
    |> Result.map ~fn:(fun _ -> ())
  in
  let emit_materialization_events =
    should_emit_source_materialization_started ~source_locator:parsed.source_locator ~update
  in
  if emit_materialization_events then
    emit
      (Riot_model.Event.DepsSourceMaterializationStarted {
        source_locator = parsed.source_locator;
        ref_ = parsed.ref_;
      });
  let* materialized =
    Git_dependency.materialize ~update ~source_locator:parsed.source_locator ~ref_:parsed.ref_ ()
    |> Result.map_err
      ~fn:(fun error ->
        SourceDependencyLoadFailed {
          dependency = source_spec;
          source_locator = parsed.source_locator;
          ref_ = parsed.ref_;
          error = SourceDependencyMaterializationFailed error;
        })
  in
  let* registry = init_registry () in
  let loaded =
    match decode_detached_package ~package_root:materialized.package_root with
    | Ok package ->
        ensure_loaded_workspace
          ~workspace_manager
          ~registry
          ~workspace:(Riot_model.Workspace_manifest.make
            ~root:materialized.package_root
            ~packages:[ package ]
            ())
          ~package_name:package.name
          ()
    | Error _ ->
        let* workspace =
          scan_workspace_from_root ~workspace_manager ~package_root:materialized.package_root ()
        in
        let* package_name =
          select_materialized_package ~workspace ~package_root:materialized.package_root ()
        in
        ensure_loaded_workspace ~workspace_manager ~registry ~workspace ~package_name ()
  in
  let* loaded = loaded in
  let selected_package =
    List.find
      loaded.workspace.packages
      ~fn:(fun (pkg: Riot_model.Package_manifest.t) ->
        Riot_model.Package_name.equal
          pkg.name
          loaded.package_name)
  in
  if emit_materialization_events then
    emit
      (
        Riot_model.Event.DepsSourceMaterializationFinished {
          source_locator = parsed.source_locator;
          ref_ = parsed.ref_;
          package = loaded.package_name;
          version =
            match selected_package with
            | Some pkg -> Option.map pkg.publish.version ~fn:Std.Version.to_string
            | None ->
                None;
        }
      );
  emit
    (
      Riot_model.Event.DepsPackageResolvedForBuild {
        package = loaded.package_name;
        version =
          (
            match selected_package with
            | Some pkg -> Option.map pkg.publish.version ~fn:Std.Version.to_string
            | None -> None
          );
        path = Path.to_string materialized.package_root;
        workspace = false;
      }
    );
  Ok loaded

let load_source_workspace = fun ?(emit = no_emit) ~workspace_manager ?(update = true) ~spec () ->
  let* parsed =
    Git_dependency.parse_spec spec
    |> Result.map_err
      ~fn:(fun error ->
        DependencySpecInvalid { dependency = spec; error = SourceDependencySpecError error })
  in
  load_source_workspace_from_spec ~workspace_manager ~emit ~update ~spec:parsed ()

let load_registry_workspace_from_spec = fun
  ?(emit = no_emit) ?registry ~workspace_manager ~spec () ->
  let parsed = spec in
  let package_name = registry_dependency_name parsed in
  let package_name_string = Riot_model.Package_name.to_string package_name in
  let* registry =
    match registry with
    | Some registry -> Ok registry
    | None -> init_registry ()
  in
  let* parsed = lookup_named_package ~emit ~registry parsed in
  let registry_name = Pkgs_ml.Registry.name registry in
  let requirement = Option.unwrap_or ~default:Std.Version.any parsed.requirement in
  let* document =
    Pkgs_ml.Registry.read_package_document registry ~package_name:package_name_string
    |> Result.map_err
      ~fn:(fun error ->
        RegistryLookupFailed {
          package = package_name_string;
          registry = registry_name;
          error = RegistryPackageDocumentReadFailed error;
        })
  in
  let* document =
    match document with
    | Some document -> Ok document
    | None ->
        Error (RegistryPackageNotFound {
          package = package_name_string;
          registry = registry_name;
          suggestions = lookup_package_suggestions ~registry ~package_name:package_name_string;
        })
  in
  let* release =
    match matching_release_of_document document requirement with
    | Some release -> Ok release
    | None -> (
        match yanked_release_of_document document requirement with
        | Some release ->
            Error (RegistryReleaseYanked {
              package = package_name_string;
              version = release.version;
              registry = registry_name;
            })
        | None ->
            Error (RegistryVersionNotFound {
              package = package_name_string;
              requirement = Std.Version.requirement_to_string requirement;
              registry = registry_name;
            })
      )
  in
  let lock_package =
    Riot_model.Lockfile.{
      id =
        {
          registry = Some registry_name;
          name = package_name;
          version = Some release.version;
          sha256 = Some release.artifact_sha256;
        };
      root = None;
      provenance = Registry { registry = registry_name };
      dependencies = [];
      build_dependencies = [];
      dev_dependencies = [];
    }
  in
  let* package_root =
    Materializer.ensure_registry_package ~emit ~registry ~pkg:lock_package ()
    |> Result.map_err
      ~fn:(fun err ->
        RegistryMaterializationFailed {
          package = package_name_string;
          version = release.version;
          registry = registry_name;
          error = RegistryPackageMaterializationFailed err;
        })
  in
  let manifest_path = Path.(package_root / Path.v "riot.toml") in
  let* manifest_source =
    Fs.read_to_string manifest_path
    |> Result.map_err
      ~fn:(fun err ->
        RegistryMaterializationFailed {
          package = package_name_string;
          version = release.version;
          registry = registry_name;
          error = RegistryPackageManifestReadFailed err;
        })
  in
  let* toml =
    Data.Toml.parse manifest_source
    |> Result.map_err
      ~fn:(fun err ->
        RegistryMaterializationFailed {
          package = package_name_string;
          version = release.version;
          registry = registry_name;
          error = RegistryPackageTomlParseFailed err;
        })
  in
  let* package =
    Riot_model.Package_manifest.from_toml
      toml
      ~workspace_deps:[]
      ~workspace_dev_deps:[]
      ~workspace_build_deps:[]
      ~path:package_root
      ~relative_path:(Path.v ".")
    |> Result.map_err
      ~fn:(fun err ->
        RegistryMaterializationFailed {
          package = package_name_string;
          version = release.version;
          registry = registry_name;
          error = RegistryPackageManifestDecodeFailed err;
        })
  in
  let* loaded =
    ensure_loaded_workspace
      ~workspace_manager
      ~registry
      ~workspace:(Riot_model.Workspace_manifest.make ~root:package_root ~packages:[ package ] ())
      ~package_name:package.name
      ()
  in
  emit
    (
      Riot_model.Event.DepsPackageResolvedForBuild {
        package = loaded.package_name;
        version = Some release.version;
        path = Path.to_string package_root;
        workspace = false;
      }
    );
  Ok loaded

let load_registry_workspace = fun ?(emit = no_emit) ?registry ~workspace_manager ~spec () ->
  let* parsed = parse_registry_dependency_spec spec in
  load_registry_workspace_from_spec ~workspace_manager ~emit ?registry ~spec:parsed ()

let upsert_dependency = fun
  (dependencies: Riot_model.Package.dependency list)
  ((dependency: Riot_model.Package.dependency) as replacement) ->
  let rec loop
    (acc: Riot_model.Package.dependency list)
    (remaining: Riot_model.Package.dependency list) =
    match remaining with
    | [] -> List.reverse (replacement :: acc)
    | current :: rest when Riot_model.Package_name.equal current.name dependency.name ->
        List.append (List.reverse acc) (replacement :: rest)
    | current :: rest -> loop (current :: acc) rest
  in
  loop [] dependencies

let remove_dependency = fun dependencies ~name ->
  let kept =
    List.filter
      dependencies
      ~fn:(fun (dep: Riot_model.Package.dependency) -> not (Package_name.equal dep.name name))
  in
  (kept, not (Int.equal (List.length kept) (List.length dependencies)))

let dependency_of_parsed = fun __tmp1 ->
  match __tmp1 with
  | Registry parsed ->
      Package.{
        name = registry_dependency_name parsed;
        source =
          {
            workspace = false;
            builtin = Package.is_builtin_dependency_name
              (Package_name.to_string (registry_dependency_name parsed));
            path = None;
            source_locator = None;
            ref_ = None;
            version = parsed.requirement;
          };
      }
  | Path parsed ->
      Package.{
        name = parsed.name;
        source =
          {
            workspace = false;
            builtin = false;
            path = Some parsed.path;
            source_locator = None;
            ref_ = None;
            version = None;
          };
      }
  | Source parsed ->
      Package.{
        name = parsed.name;
        source =
          {
            workspace = false;
            builtin = false;
            path = None;
            source_locator = Some parsed.source_locator;
            ref_ = parsed.ref_;
            version = None;
          };
      }

let reload_workspace = fun ~workspace_manager ~(workspace_root:Path.t) ->
  Riot_model.Workspace_manager.clear_cache workspace_manager;
  let* (workspace, load_errors) =
    Riot_model.Workspace_manager.scan workspace_manager workspace_root
    |> Result.map_err ~fn:(fun error -> WorkspaceReloadFailed { workspace_root; error })
  in
  match load_errors with
  | [] -> Ok workspace
  | load_errors -> Error (WorkspaceReloadHadErrors { workspace_root; errors = load_errors })

let refresh_lock = fun
  ?existing_lock
  ~workspace_manager
  ~(emit:event_sink)
  ~mode
  ~registry
  ~(workspace:Riot_model.Workspace_manifest.t)
  () ->
  Workspace_resolution.ensure_lock
    ?existing_lock
    ~workspace_manager
    ~emit
    ~mode
    ~registry
    ~workspace
    ()
  |> Result.map ~fn:(fun (lockfile, _) -> lockfile)
  |> Result.map_err ~fn:(fun error -> LockRefreshFailed error)

type lock_package_key = {
  registry: string;
  package: Package_name.t;
}

let lock_package_key = fun (pkg: Lockfile.package) ->
  match (pkg.id.registry, pkg.id.version) with
  | (Some registry, Some _) -> Some { registry; package = pkg.id.name }
  | _ -> None

let lock_package_key_equal = fun left right ->
  String.equal left.registry right.registry && Package_name.equal left.package right.package

let lock_package_version_map = fun (lockfile: Riot_model.Lockfile.t) ->
  List.fold_left
    lockfile.packages
    ~init:[]
    ~fn:(fun acc (pkg: Riot_model.Lockfile.package) ->
      match (lock_package_key pkg, pkg.id.version) with
      | (Some key, Some version) -> (key, version) :: acc
      | _ -> acc)

let emit_updated_packages = fun
  ~(emit:event_sink) ~(previous:Riot_model.Lockfile.t) (current: Riot_model.Lockfile.t) ->
  let previous_versions = lock_package_version_map previous in
  List.fold_left
    current.packages
    ~init:0
    ~fn:(fun updates (pkg: Riot_model.Lockfile.package) ->
      match (lock_package_key pkg, pkg.id.version) with
      | (Some key, Some to_version) -> (
          match List.find
            previous_versions
            ~fn:(fun (existing_key, _) -> lock_package_key_equal existing_key key)
          |> Option.map ~fn:(fun (_, version) -> version) with
          | Some from_version when not (String.equal from_version to_version) ->
              emit
                (Riot_model.Event.DepsPackageVersionUpdated {
                  package = pkg.id.name;
                  from_version;
                  to_version;
                });
              updates + 1
          | _ -> updates
        )
      | _ -> updates)

let parsed_dependency_name = fun __tmp1 ->
  match __tmp1 with
  | Registry parsed -> Package_name.to_string (registry_dependency_name parsed)
  | Path parsed -> Package_name.to_string parsed.name
  | Source parsed -> Package_name.to_string parsed.name

let update_manifest_many = fun
  ~(emit:event_sink)
  ~(target:target_manifest)
  ~scope
  ~dependencies
  ~operation
  ~updated_dependencies ->
  Manifest_edit.update_dependency_section
    ~manifest_path:target.path
    ~section:(scope_to_section scope)
    ~dependencies
  |> Result.map_err ~fn:(fun error -> ManifestUpdateFailed error)
  |> Result.map
    ~fn:(fun () ->
      List.for_each
        updated_dependencies
        ~fn:(fun dependency ->
          emit
            (
              Riot_model.Event.DepsManifestUpdated {
                path = Path.to_string target.path;
                section = Manifest_edit.section_name (scope_to_section scope);
                operation;
                dependency;
              }
            )))

let add = fun
  ?(on_event = no_emit)
  ~workspace_manager
  ~(workspace:Riot_model.Workspace_manifest.t)
  ~cwd
  ~(request:add_request)
  () ->
  let emit = on_event in
  let* target = target_manifest ~workspace ~cwd request.selection request.scope in
  let* registry = init_registry () in
  let rec parse_all acc = fun __tmp1 ->
    match __tmp1 with
    | [] -> Ok (List.reverse acc)
    | dependency :: rest ->
        let* parsed =
          if is_source_dependency_spec dependency then
            load_source_dependency ~emit ~raw:dependency
          else
            parse_dependency_spec ~target dependency
        in
        let* parsed =
          match parsed with
          | Registry parsed ->
              lookup_named_package ~emit ~registry parsed
              |> Result.map ~fn:(fun parsed -> Registry parsed)
          | Path parsed -> Ok (Path parsed)
          | Source parsed -> Ok (Source parsed)
        in
        parse_all (parsed :: acc) rest
  in
  let* parsed_dependencies = parse_all [] request.dependencies in
  let dependencies =
    List.fold_left
      parsed_dependencies
      ~init:target.dependencies
      ~fn:(fun dependencies parsed -> upsert_dependency dependencies (dependency_of_parsed parsed))
  in
  let* () =
    update_manifest_many
      ~emit
      ~target
      ~scope:request.scope
      ~dependencies
      ~operation:`Add
      ~updated_dependencies:(List.map parsed_dependencies ~fn:parsed_dependency_name)
  in
  let* workspace = reload_workspace ~workspace_manager ~workspace_root:workspace.root in
  let* _lockfile =
    refresh_lock ~workspace_manager ~emit ~mode:Dep_solver.Refresh ~registry ~workspace ()
  in
  Ok ()

let remove = fun
  ?(on_event = no_emit)
  ~workspace_manager
  ~(workspace:Riot_model.Workspace_manifest.t)
  ~cwd
  ~(request:remove_request)
  () ->
  let emit = on_event in
  let* registry = init_registry () in
  let* target = target_manifest ~workspace ~cwd request.selection request.scope in
  let rec remove_all dependencies = fun __tmp1 ->
    match __tmp1 with
    | [] -> Ok dependencies
    | dependency :: rest ->
        let (dependencies, removed) = remove_dependency dependencies ~name:dependency in
        if not removed then
          Error (DependencyNotFoundInSection {
            path = target.path;
            section = Manifest_edit.section_name (scope_to_section request.scope);
            dependency = Package_name.to_string dependency;
          })
        else
          remove_all dependencies rest
  in
  let* dependencies = remove_all target.dependencies request.dependencies in
  let* () =
    update_manifest_many
      ~emit
      ~target
      ~scope:request.scope
      ~dependencies
      ~operation:`Remove
      ~updated_dependencies:(List.map request.dependencies ~fn:Package_name.to_string)
  in
  let* workspace = reload_workspace ~workspace_manager ~workspace_root:workspace.root in
  let* _lockfile =
    refresh_lock ~workspace_manager ~emit ~mode:Dep_solver.Refresh ~registry ~workspace ()
  in
  Ok ()

let package_requested_for_update = fun requested (pkg: Riot_model.Lockfile.package) ->
  List.contains
    requested
    ~value:pkg.id.name

let existing_lock_for_targeted_update = fun requested (lockfile: Riot_model.Lockfile.t) ->
  Riot_model.Lockfile.{
    lockfile with
    dependency_hash = "";
    packages = List.filter
      lockfile.packages
      ~fn:(fun pkg -> not (package_requested_for_update requested pkg));
  }

let update = fun
  ?(on_event = no_emit)
  ?registry
  ~workspace_manager
  ~(workspace:Riot_model.Workspace_manifest.t)
  ~(request:update_request)
  () ->
  let emit = on_event in
  let* registry =
    match registry with
    | Some registry -> Ok registry
    | None -> init_registry ()
  in
  let previous_lock =
    match Lockfile_store.read ~workspace_root:workspace.root with
    | Ok lockfile -> lockfile
    | Error _ -> None
  in
  let* scanned_workspace = reload_workspace ~workspace_manager ~workspace_root:workspace.root in
  let targeted_existing_lock =
    match (request.packages, previous_lock) with
    | ([], _) -> None
    | (_, None) -> Some None
    | (packages, Some lockfile) -> Some (Some (existing_lock_for_targeted_update packages lockfile))
  in
  let* lockfile =
    refresh_lock
      ?existing_lock:targeted_existing_lock
      ~workspace_manager
      ~emit
      ~mode:(
        if List.is_empty request.packages then
          Dep_solver.Unlock
        else
          Dep_solver.Refresh
      )
      ~registry
      ~workspace:scanned_workspace
      ()
  in
  Option.for_each
    previous_lock
    ~fn:(fun previous ->
      let updates = emit_updated_packages ~emit ~previous lockfile in
      if Int.equal updates 0 then
        emit
          (
            Riot_model.Event.DepsPackageVersionsUnchanged {
              packages =
                if List.is_empty request.packages then
                  List.length lockfile.packages
                else
                  List.length request.packages;
            }
          ));
  Ok ()
