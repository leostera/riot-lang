open Std

let ( let* ) = Result.and_then

let duration_ms_since = fun started ->
  Time.Instant.duration_since ~earlier:started (Time.Instant.now ()) |> Time.Duration.to_millis

type dependency_scope =
  | Runtime
  | Build
  | Dev

type manifest_selection =
  | Current
  | Workspace
  | Package of string

type suggested_package = {
  package: string;
  latest_version: string;
  description: string option;
}

type search_request = {
  query: string;
  limit: int;
}

type loaded_workspace = {
  workspace: Riot_model.Workspace.t;
  package_name: string;
}

type event_sink = Riot_model.Event.kind -> unit

type add_request = {
  selection: manifest_selection;
  scope: dependency_scope;
  dependency: string;
}

type remove_request = {
  selection: manifest_selection;
  scope: dependency_scope;
  dependency: string;
}

type error =
  | CurrentPackageNotFound of { cwd: Path.t }
  | PackageNotFound of { package: string }
  | DependencySpecInvalid of { dependency: string; error: string }
  | PathDependencyMustBeRelative of { dependency: string }
  | PathDependencyLoadFailed of { dependency: string; path: Path.t; error: string }
  | SourceDependencyLoadFailed of {
      dependency: string;
      source_locator: string;
      ref_: string option;
      error: string
    }
  | RegistryInitializationFailed of { registry: string; error: string }
  | RegistryLookupFailed of { package: string; registry: string; error: string }
  | RegistryMaterializationFailed of {
      package: string;
      version: string;
      registry: string;
      error: string
    }
  | RegistrySearchFailed of { query: string; registry: string; error: string }
  | RegistryPackageNotFound of {
      package: string;
      registry: string;
      suggestions: suggested_package list
    }
  | RegistryReleaseYanked of { package: string; version: string; registry: string }
  | RegistryVersionNotFound of { package: string; requirement: string; registry: string }
  | ManifestUpdateFailed of { path: Path.t; error: string }
  | DependencyNotFoundInSection of { path: Path.t; section: string; dependency: string }
  | WorkspaceReloadFailed of { workspace_root: Path.t; error: string }
  | WorkspaceReloadHadErrors of { workspace_root: Path.t; errors: string list }
  | MaterializedPackageNotFound of { package_root: Path.t; workspace_root: Path.t }
  | LockRefreshFailed of Error.t

type target_manifest = {
  path: Path.t;
  dependencies: Riot_model.Package.dependency list;
}

type registry_dependency = {
  name: string;
  requirement: Std.Version.requirement option;
}

type path_dependency = {
  name: string;
  path: Path.t;
}

type source_dependency = {
  name: string;
  source_locator: string;
  ref_: string option;
}

type parsed_dependency =
  | Registry of registry_dependency
  | Path of path_dependency
  | Source of source_dependency

let no_emit: event_sink = fun _ -> ()

let registry_name = "pkgs.ml"

let should_emit_source_materialization_started = fun ~source_locator ~update ->
  if update then
    true
  else
    match Git_dependency.parse_source_locator source_locator with
    | Error _ -> true
    | Ok locator -> (
        let repo_dir = Riot_model.Riot_dirs.git_registry_repo_dir
          ~host:locator.host
          ~owner:locator.owner
          ~repo:locator.repo in
        match Fs.exists repo_dir with
        | Ok exists -> not exists
        | Error _ -> true
      )

let error_message = function
  | CurrentPackageNotFound { cwd } ->
      "could not determine current package from '" ^ Path.to_string cwd ^ "'"
  | PackageNotFound { package } ->
      "workspace package '" ^ package ^ "' was not found"
  | DependencySpecInvalid { dependency; error } ->
      "invalid dependency '" ^ dependency ^ "': " ^ error
  | PathDependencyMustBeRelative { dependency } ->
      "path dependency '" ^ dependency ^ "' must be a relative path"
  | PathDependencyLoadFailed { dependency; path; error } ->
      "failed to load path dependency '" ^ dependency ^ "' from '" ^ Path.to_string path ^ "': " ^ error
  | SourceDependencyLoadFailed { dependency; source_locator; ref_; error } ->
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
      ^ error
  | RegistryInitializationFailed { registry; error } ->
      "failed to initialize registry '" ^ registry ^ "': " ^ error
  | RegistryLookupFailed { package; registry; error } ->
      "failed to look up package '" ^ package ^ "' in registry '" ^ registry ^ "': " ^ error
  | RegistryMaterializationFailed { package; version; registry; error } ->
      "failed to materialize package '"
      ^ package
      ^ "@"
      ^ version
      ^ "' from registry '"
      ^ registry
      ^ "': "
      ^ error
  | RegistrySearchFailed { query; registry; error } ->
      "failed to search registry '" ^ registry ^ "' for '" ^ query ^ "': " ^ error
  | RegistryPackageNotFound { package; registry; suggestions } ->
      let base = "package '" ^ package ^ "' was not found in registry '" ^ registry ^ "'" in
      (
        match suggestions with
        | [] -> base
        | suggestions ->
            let lines =
              List.map
                (fun { package; latest_version; description } ->
                  match description with
                  | Some description -> "  - " ^ package ^ "@" ^ latest_version ^ " - " ^ description
                  | None -> "  - " ^ package ^ "@" ^ latest_version)
                suggestions
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
  | ManifestUpdateFailed { path; error } ->
      "failed to update manifest '" ^ Path.to_string path ^ "': " ^ error
  | DependencyNotFoundInSection { path; section; dependency } ->
      "dependency '"
      ^ dependency
      ^ "' was not found in ["
      ^ section
      ^ "] of '"
      ^ Path.to_string path
      ^ "'"
  | WorkspaceReloadFailed { workspace_root; error } ->
      "failed to reload workspace '" ^ Path.to_string workspace_root ^ "': " ^ error
  | WorkspaceReloadHadErrors { workspace_root; errors } ->
      "workspace '" ^ Path.to_string workspace_root ^ "' has load errors:\n" ^ String.concat "\n" errors
  | MaterializedPackageNotFound { package_root; workspace_root } ->
      "materialized package root '"
      ^ Path.to_string package_root
      ^ "' does not correspond to a package in workspace '"
      ^ Path.to_string workspace_root
      ^ "'"
  | LockRefreshFailed error ->
      Riot_model.Pm_error.message error

let scope_to_section = function
  | Runtime -> Manifest_edit.Runtime
  | Build -> Manifest_edit.Build
  | Dev -> Manifest_edit.Dev

let parse_registry_dependency_spec = fun raw ->
  match String.split_on_char '@' raw with
  | [ name ] ->
      let* name = Riot_model.Package.validate_name (String.trim name)
      |> Result.map_error (fun error -> DependencySpecInvalid { dependency = raw; error }) in
      Ok { name; requirement = Some Std.Version.any }
  | [name;requirement] ->
      let* name = Riot_model.Package.validate_name (String.trim name)
      |> Result.map_error (fun error -> DependencySpecInvalid { dependency = raw; error }) in
      let requirement = String.trim requirement in
      let* requirement =
        Std.Version.parse_requirement requirement |> Result.map_error
          (fun error ->
            DependencySpecInvalid {
              dependency = raw;
              error =
                match error with
                | Std.Version.Invalid_format msg -> msg
                | Std.Version.Invalid_version_segment segment -> "invalid version segment: " ^ segment
                | Std.Version.Invalid_pre_release_segment segment -> "invalid pre-release segment: "
                ^ segment;
            })
      in
      Ok { name; requirement = Some requirement }
  | _ ->
      Error (DependencySpecInvalid {
        dependency = raw;
        error = "expected <name> or <name>@<version>"
      })

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
    let* source = Fs.read_to_string manifest_path
    |> Result.map_error
      (fun err ->
        PathDependencyLoadFailed {
          dependency = raw;
          path = package_root;
          error = IO.error_message err
        }) in
    let* toml = Data.Toml.parse source
    |> Result.map_error
      (fun err ->
        PathDependencyLoadFailed {
          dependency = raw;
          path = package_root;
          error = Data.Toml.error_to_string err
        }) in
    let* package = Riot_model.Package.from_toml
      toml
      ~workspace_deps:[]
      ~workspace_dev_deps:[]
      ~workspace_build_deps:[]
      ~path:package_root
      ~relative_path:dep_path
    |> Result.map_error
      (fun err -> PathDependencyLoadFailed { dependency = raw; path = package_root; error = err }) in
    Ok (Path { name = package.name; path = dep_path })

let load_source_dependency = fun ~(emit:event_sink) ~raw ->
  let* spec = Git_dependency.parse_spec raw
  |> Result.map_error
    (fun error -> DependencySpecInvalid { dependency = raw; error = Git_dependency.message error }) in
  let* () = Git_dependency.parse_source_locator spec.source_locator
  |> Result.map_error
    (fun error -> DependencySpecInvalid { dependency = raw; error = Git_dependency.message error })
  |> Result.map (fun _ -> ()) in
  let emit_materialization_events = should_emit_source_materialization_started
    ~source_locator:spec.source_locator
    ~update:true in
  if emit_materialization_events then
    emit
      (Riot_model.Event.SourceDependencyMaterializationStarted {
        source_locator = spec.source_locator;
        ref_ = spec.ref_
      });
  let* materialized = Git_dependency.materialize
    ~source_locator:spec.source_locator
    ~ref_:spec.ref_
    ()
  |> Result.map_error
    (fun error ->
      SourceDependencyLoadFailed {
        dependency = raw;
        source_locator = spec.source_locator;
        ref_ = spec.ref_;
        error = Git_dependency.message error
      }) in
  let manifest_path = Path.(materialized.package_root / Path.v "riot.toml") in
  let* source = Fs.read_to_string manifest_path
  |> Result.map_error
    (fun err ->
      SourceDependencyLoadFailed {
        dependency = raw;
        source_locator = spec.source_locator;
        ref_ = spec.ref_;
        error = IO.error_message err
      }) in
  let* toml = Data.Toml.parse source
  |> Result.map_error
    (fun err ->
      SourceDependencyLoadFailed {
        dependency = raw;
        source_locator = spec.source_locator;
        ref_ = spec.ref_;
        error = Data.Toml.error_to_string err
      }) in
  let relative_path =
    match Path.strip_prefix materialized.package_root ~prefix:materialized.repository_root with
    | Ok relative_path -> relative_path
    | Error _ -> Path.v "."
  in
  let* package = Riot_model.Package.from_toml
    toml
    ~workspace_deps:[]
    ~workspace_dev_deps:[]
    ~workspace_build_deps:[]
    ~path:materialized.package_root
    ~relative_path
  |> Result.map_error
    (fun error ->
      SourceDependencyLoadFailed {
        dependency = raw;
        source_locator = spec.source_locator;
        ref_ = spec.ref_;
        error
      }) in
  if emit_materialization_events then
    emit
      (Riot_model.Event.SourceDependencyMaterializationFinished {
        source_locator = spec.source_locator;
        ref_ = spec.ref_;
        package = package.name;
        version = Option.map Std.Version.to_string package.publish.version
      });
  Ok (Source { name = package.name; source_locator = spec.source_locator; ref_ = spec.ref_ })

let parse_dependency_spec = fun ~(target:target_manifest) raw ->
  if is_source_dependency_spec raw then
    load_source_dependency ~emit:no_emit ~raw
  else
    match parse_registry_dependency_spec raw with
    | Ok parsed -> Ok (Registry parsed)
    | Error _ when String.contains raw "/" || String.starts_with ~prefix:"." raw -> load_path_dependency
      ~target
      ~raw
    | Error err -> Error err

let init_registry = fun () ->
  Pkgs_ml.Registry.create_filesystem ~registry_name ()
  |> Result.map_error (fun error -> RegistryInitializationFailed { registry = registry_name; error })

let workspace_manager_or_create = function
  | Some workspace_manager -> workspace_manager
  | None -> Riot_model.Workspace_manager.create ()

let select_materialized_package = fun ~(workspace:Riot_model.Workspace.t) ?preferred_package_name ~package_root () ->
  match Riot_model.Workspace.find_package_for_path workspace ~path:package_root with
  | Some pkg -> Ok pkg.name
  | None -> (
      match preferred_package_name with
      | Some preferred_package_name -> (
          match
            List.find_opt
              (fun (pkg: Riot_model.Package.t) ->
                String.equal pkg.name preferred_package_name)
              workspace.packages
          with
          | Some pkg -> Ok pkg.name
          | None ->
              match workspace.packages with
              | [ pkg ] -> Ok pkg.name
              | _ -> (
                  match List.filter Riot_model.Package.is_workspace_member workspace.packages with
                  | [ pkg ] -> Ok pkg.name
                  | _ -> Error (MaterializedPackageNotFound {
                    package_root;
                    workspace_root = workspace.root
                  })
                )
        )
      | None ->
          match workspace.packages with
          | [ pkg ] -> Ok pkg.name
          | _ -> (
              match List.filter Riot_model.Package.is_workspace_member workspace.packages with
              | [ pkg ] -> Ok pkg.name
              | _ -> Error (MaterializedPackageNotFound {
                package_root;
                workspace_root = workspace.root
              })
            )
    )

let scan_workspace_from_root = fun ?workspace_manager ~package_root () ->
  let workspace_manager = workspace_manager_or_create workspace_manager in
  let* (workspace, load_errors) = Riot_model.Workspace_manager.scan workspace_manager package_root
  |> Result.map_error (fun error -> WorkspaceReloadFailed { workspace_root = package_root; error }) in
  match load_errors with
  | [] -> Ok workspace
  | load_errors ->
      let errors = List.map Riot_model.Workspace_manager.load_error_to_string load_errors in
      Error (WorkspaceReloadHadErrors { workspace_root = workspace.root; errors })

let matching_release_of_document = fun (document: Pkgs_ml.Sparse_index.package_document) requirement ->
  let matches =
    List.filter_map
      (fun (release: Pkgs_ml.Sparse_index.release) ->
        match Std.Version.parse release.version with
        | Ok _ when release.yanked -> None
        | Ok version when Std.Version.matches requirement version -> Some (version, release)
        | Ok _
        | Error _ -> None)
      document.releases
  in
  match
    List.sort
      (fun (left, _) (right, _) ->
        match Std.Version.compare left right with
        | Lt -> 1
        | Eq -> 0
        | Gt -> (-1))
      matches
  with
  | (_, release) :: _ -> Some release
  | [] -> None

let decode_detached_package = fun ~package_root ->
  let manifest_path = Path.(package_root / Path.v "riot.toml") in
  let* source = Fs.read_to_string manifest_path |> Result.map_error (fun err -> IO.error_message err) in
  let* toml = Data.Toml.parse source |> Result.map_error Data.Toml.error_to_string in
  Riot_model.Package.from_toml
    toml
    ~workspace_deps:[]
    ~workspace_dev_deps:[]
    ~workspace_build_deps:[]
    ~path:package_root
    ~relative_path:(Path.v ".")

let dependency_lists_for_package = fun scope (pkg: Riot_model.Package.t) ->
  match scope with
  | Runtime -> pkg.dependencies
  | Build -> pkg.build_dependencies
  | Dev -> pkg.dev_dependencies

let dependency_lists_for_workspace = fun scope (workspace: Riot_model.Workspace.t) ->
  match scope with
  | Runtime -> workspace.dependencies
  | Build -> workspace.build_dependencies
  | Dev -> workspace.dev_dependencies

let dependency_names_for_section = fun ~manifest_path ~section ->
  let section_name = Manifest_edit.section_name (scope_to_section section) in
  let* source = Fs.read_to_string manifest_path
  |> Result.map_error
    (fun err ->
      ManifestUpdateFailed {
        path = manifest_path;
        error = "failed to read manifest: " ^ IO.error_message err
      }) in
  let* toml = Data.Toml.parse source
  |> Result.map_error
    (fun err ->
      ManifestUpdateFailed {
        path = manifest_path;
        error = "failed to parse manifest TOML: " ^ Data.Toml.error_to_string err
      }) in
  match toml with
  | Data.Toml.Table fields -> (
      match List.assoc_opt section_name fields with
      | None -> Ok []
      | Some (Data.Toml.Table dep_items) -> Ok (List.map fst dep_items)
      | Some _ -> Error (ManifestUpdateFailed {
        path = manifest_path;
        error = "[" ^ section_name ^ "] must be a table"
      })
    )
  | _ -> Error (ManifestUpdateFailed {
    path = manifest_path;
    error = "manifest root must be a TOML table"
  })

let filter_dependencies_by_names = fun names dependencies ->
  List.filter
    (fun (dep: Riot_model.Package.dependency) ->
      List.mem dep.name names)
    dependencies

let select_current_package = fun ~(workspace:Riot_model.Workspace.t) ~cwd ->
  match Riot_model.Workspace.find_package_for_path workspace ~path:cwd with
  | Some pkg -> Ok pkg
  | None -> (
      match List.filter Riot_model.Package.is_workspace_member workspace.packages with
      | [ pkg ] -> Ok pkg
      | _ -> Error (CurrentPackageNotFound { cwd })
    )

let target_manifest = fun ~(workspace:Riot_model.Workspace.t) ~cwd selection scope ->
  match selection with
  | Workspace ->
      let path = Path.(workspace.root / Path.v "riot.toml") in
      let* names = dependency_names_for_section ~manifest_path:path ~section:scope in
      Ok {
        path;
        dependencies = filter_dependencies_by_names
          names
          (dependency_lists_for_workspace scope workspace)
      }
  | Package package_name -> (
      match
        workspace.packages |> List.filter Riot_model.Package.is_workspace_member |> List.find_opt
          (fun (pkg: Riot_model.Package.t) ->
            String.equal pkg.name package_name)
      with
      | Some pkg ->
          let path = Path.(pkg.path / Path.v "riot.toml") in
          let* names = dependency_names_for_section ~manifest_path:path ~section:scope in
          Ok {
            path;
            dependencies = filter_dependencies_by_names
              names
              (dependency_lists_for_package scope pkg)
          }
      | None -> Error (PackageNotFound { package = package_name })
    )
  | Current ->
      let* pkg = select_current_package ~workspace ~cwd in
      let path = Path.(pkg.path / Path.v "riot.toml") in
      let* names = dependency_names_for_section ~manifest_path:path ~section:scope in
      Ok {
        path;
        dependencies = filter_dependencies_by_names names (dependency_lists_for_package scope pkg)
      }

let dependency_exists = fun ~(package_name:string) document requirement ->
  let rec loop = function
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

let yanked_release_of_document = fun (document: Pkgs_ml.Sparse_index.package_document) requirement ->
  List.find_opt
    (fun (release: Pkgs_ml.Sparse_index.release) ->
      if not release.yanked then
        false
      else
        match Std.Version.parse release.version with
        | Ok version -> Std.Version.matches requirement version
        | Error _ -> false)
    document.releases

let suggested_package_of_search_result = fun (result: Pkgs_ml.Registry.search_result) ->
  {
    package = result.package_name;
    latest_version = result.latest_version;
    description = result.description
  }

let lookup_package_suggestions = fun ~registry ~package_name ->
  match Pkgs_ml.Registry.search_packages registry ~query:package_name ~limit:5 () with
  | Ok results -> results
  |> List.filter
    (fun (result: Pkgs_ml.Registry.search_result) ->
      not
        (String.equal
          (Pkgs_ml.Sparse_index.normalized_name result.package_name)
          (Pkgs_ml.Sparse_index.normalized_name package_name)))
  |> List.map suggested_package_of_search_result
  | Error _ -> []

let search = fun ?registry ~(request:search_request) () ->
  let* registry =
    match registry with
    | Some registry -> Ok registry
    | None -> init_registry ()
  in
  let* results = Pkgs_ml.Registry.search_packages
    registry
    ~query:request.query
    ~limit:request.limit
    ()
  |> Result.map_error
    (fun error ->
      RegistrySearchFailed {
        query = request.query;
        registry = Pkgs_ml.Registry.name registry;
        error
      }) in
  Ok (List.map suggested_package_of_search_result results)

let lookup_named_package = fun ~(emit:event_sink) ~registry (parsed: registry_dependency) ->
  let registry_name = Pkgs_ml.Registry.name registry in
  let started = Time.Instant.now () in
  emit
    (Riot_model.Event.PackageMetadataFetchStarted { registry = registry_name; package = parsed.name });
  let* document =
    Pkgs_ml.Registry.read_package_document registry ~package_name:parsed.name |> Result.map_error
      (fun error ->
        emit
          (Riot_model.Event.PackageMetadataFetchFailed {
            registry = registry_name;
            package = parsed.name;
            error = Riot_model.Pm_error.PackageMetadataReadFailed {
              package = parsed.name;
              registry = registry_name;
              error
            }
          });
        RegistryLookupFailed { package = parsed.name; registry = registry_name; error })
  in
  match document with
  | None ->
      emit
        (Riot_model.Event.PackageMetadataFetchFailed {
          registry = registry_name;
          package = parsed.name;
          error = Riot_model.Pm_error.PackageNotFound {
            package = parsed.name;
            registry = registry_name;
            required_by = None
          }
        });
      Error (RegistryPackageNotFound {
        package = parsed.name;
        registry = registry_name;
        suggestions = lookup_package_suggestions ~registry ~package_name:parsed.name
      })
  | Some document ->
      emit
        (Riot_model.Event.PackageMetadataFetchFinished {
          registry = registry_name;
          package = document.name;
          version = Some document.latest;
          duration_ms = duration_ms_since started
        });
      let requirement = Option.unwrap_or ~default:Std.Version.any parsed.requirement in
      if dependency_exists ~package_name:parsed.name document requirement then
        Ok parsed
      else
        (
          match yanked_release_of_document document requirement with
          | Some release -> Error (RegistryReleaseYanked {
            package = parsed.name;
            version = release.version;
            registry = registry_name
          })
          | None -> Error (RegistryVersionNotFound {
            package = parsed.name;
            requirement = Std.Version.requirement_to_string requirement;
            registry = registry_name
          })
        )

let load_source_workspace = fun ?(emit = no_emit) ?workspace_manager ?(update = true) ~spec () ->
  let* parsed = Git_dependency.parse_spec spec
  |> Result.map_error
    (fun error -> DependencySpecInvalid { dependency = spec; error = Git_dependency.message error }) in
  let* () = Git_dependency.parse_source_locator parsed.source_locator
  |> Result.map_error
    (fun error -> DependencySpecInvalid { dependency = spec; error = Git_dependency.message error })
  |> Result.map (fun _ -> ()) in
  let emit_materialization_events = should_emit_source_materialization_started
    ~source_locator:parsed.source_locator
    ~update in
  if emit_materialization_events then
    emit
      (Riot_model.Event.SourceDependencyMaterializationStarted {
        source_locator = parsed.source_locator;
        ref_ = parsed.ref_
      });
  let* materialized = Git_dependency.materialize
    ~update
    ~source_locator:parsed.source_locator
    ~ref_:parsed.ref_
    ()
  |> Result.map_error
    (fun error ->
      SourceDependencyLoadFailed {
        dependency = spec;
        source_locator = parsed.source_locator;
        ref_ = parsed.ref_;
        error = Git_dependency.message error
      }) in
  let loaded =
    match decode_detached_package ~package_root:materialized.package_root with
    | Ok package -> Ok {
      workspace = Riot_model.Workspace.make ~root:materialized.package_root ~packages:[ package ] ();
      package_name = package.name
    }
    | Error _ -> (
        let* workspace = scan_workspace_from_root
          ?workspace_manager
          ~package_root:materialized.package_root
          () in
        let* package_name = select_materialized_package
          ~workspace
          ~package_root:materialized.package_root
          () in
        Ok { workspace; package_name }
      )
  in
  let* loaded = loaded in
  let selected_package =
    List.find_opt
      (fun (pkg: Riot_model.Package.t) ->
        String.equal pkg.name loaded.package_name)
      loaded.workspace.packages
  in
  if emit_materialization_events then
    emit
      (
        Riot_model.Event.SourceDependencyMaterializationFinished {
          source_locator = parsed.source_locator;
          ref_ = parsed.ref_;
          package = loaded.package_name;
          version =
            match selected_package with
            | Some pkg -> Option.map Std.Version.to_string pkg.publish.version
            | None -> None;
        }
      );
  emit
    (
      Riot_model.Event.PackageResolvedForBuild {
        package = loaded.package_name;
        version =
          (
            match selected_package with
            | Some pkg -> Option.map Std.Version.to_string pkg.publish.version
            | None -> None
          );
        path = Path.to_string materialized.package_root;
        workspace = false;
      }
    );
  Ok loaded

let load_registry_workspace = fun ?(emit = no_emit) ?registry ?workspace_manager ~spec () ->
  let* parsed = parse_registry_dependency_spec spec in
  let* registry =
    match registry with
    | Some registry -> Ok registry
    | None -> init_registry ()
  in
  let* parsed = lookup_named_package ~emit ~registry parsed in
  let registry_name = Pkgs_ml.Registry.name registry in
  let requirement = Option.unwrap_or ~default:Std.Version.any parsed.requirement in
  let* document = Pkgs_ml.Registry.read_package_document registry ~package_name:parsed.name
  |> Result.map_error
    (fun error -> RegistryLookupFailed { package = parsed.name; registry = registry_name; error }) in
  let* document =
    match document with
    | Some document -> Ok document
    | None -> Error (RegistryPackageNotFound {
      package = parsed.name;
      registry = registry_name;
      suggestions = lookup_package_suggestions ~registry ~package_name:parsed.name
    })
  in
  let* release =
    match matching_release_of_document document requirement with
    | Some release -> Ok release
    | None -> (
        match yanked_release_of_document document requirement with
        | Some release -> Error (RegistryReleaseYanked {
          package = parsed.name;
          version = release.version;
          registry = registry_name
        })
        | None -> Error (RegistryVersionNotFound {
          package = parsed.name;
          requirement = Std.Version.requirement_to_string requirement;
          registry = registry_name
        })
      )
  in
  let lock_package =
    Riot_model.Lockfile.{
      id = {
        registry = Some registry_name;
        name = document.name;
        version = Some release.version;
        sha256 = Some release.artifact_sha256
      };
      root = None;
      provenance = Registry { registry = registry_name };
      dependencies = [];
      build_dependencies = [];
      dev_dependencies = [];
    }
  in
  let* package_root = Materializer.ensure_registry_package ~emit ~registry ~pkg:lock_package ()
  |> Result.map_error
    (fun err ->
      RegistryMaterializationFailed {
        package = document.name;
        version = release.version;
        registry = registry_name;
        error = Riot_model.Pm_error.message err
      }) in
  let manifest_path = Path.(package_root / Path.v "riot.toml") in
  let* manifest_source = Fs.read_to_string manifest_path
  |> Result.map_error
    (fun err ->
      RegistryMaterializationFailed {
        package = document.name;
        version = release.version;
        registry = registry_name;
        error = IO.error_message err
      }) in
  let* toml = Data.Toml.parse manifest_source
  |> Result.map_error
    (fun err ->
      RegistryMaterializationFailed {
        package = document.name;
        version = release.version;
        registry = registry_name;
        error = Data.Toml.error_to_string err
      }) in
  let* package = Riot_model.Package.from_toml
    toml
    ~workspace_deps:[]
    ~workspace_dev_deps:[]
    ~workspace_build_deps:[]
    ~path:package_root
    ~relative_path:(Path.v ".")
  |> Result.map_error
    (fun err ->
      RegistryMaterializationFailed {
        package = document.name;
        version = release.version;
        registry = registry_name;
        error = err
      }) in
  let workspace = Riot_model.Workspace.make ~root:package_root ~packages:[ package ] () in
  let loaded = { workspace; package_name = package.name } in
  emit
    (Riot_model.Event.PackageResolvedForBuild {
      package = loaded.package_name;
      version = Some release.version;
      path = Path.to_string package_root;
      workspace = false
    });
  Ok loaded

let upsert_dependency = fun (dependencies: Riot_model.Package.dependency list) ((
  dependency: Riot_model.Package.dependency
) as replacement) ->
  let rec loop (acc: Riot_model.Package.dependency list) (
    remaining: Riot_model.Package.dependency list
  ) =
    match remaining with
    | [] -> List.rev (replacement :: acc)
    | current :: rest when String.equal current.name dependency.name -> List.rev_append
      acc
      (replacement :: rest)
    | current :: rest -> loop (current :: acc) rest
  in
  loop [] dependencies

let remove_dependency = fun dependencies ~name ->
  let kept =
    List.filter (fun (dep: Riot_model.Package.dependency) -> not (String.equal dep.name name)) dependencies
  in
  (kept, not (Int.equal (List.length kept) (List.length dependencies)))

let dependency_of_parsed = function
  | Registry parsed ->
      Riot_model.Package.{
        name = parsed.name;
        source =
          {
            workspace = false;
            builtin = Riot_model.Package.is_builtin_dependency_name parsed.name;
            path = None;
            source_locator = None;
            ref_ = None;
            version = parsed.requirement;
          };
      }
  | Path parsed ->
      Riot_model.Package.{
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
      Riot_model.Package.{
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

let reload_workspace = fun ~(workspace_root:Path.t) ->
  let workspace_manager = Riot_model.Workspace_manager.create () in
  let* (workspace, load_errors) = Riot_model.Workspace_manager.scan workspace_manager workspace_root
  |> Result.map_error (fun error -> WorkspaceReloadFailed { workspace_root; error }) in
  match load_errors with
  | [] -> Ok workspace
  | load_errors ->
      let errors = List.map Riot_model.Workspace_manager.load_error_to_string load_errors in
      Error (WorkspaceReloadHadErrors { workspace_root; errors })

let refresh_lock = fun ~(emit:event_sink) ~mode ~registry ~(workspace:Riot_model.Workspace.t) ->
  Workspace_resolution.ensure_lock ~emit ~mode ~registry ~workspace ()
  |> Result.map fst
  |> Result.map_error (fun error -> LockRefreshFailed error)

let lock_package_version_map = fun (lockfile: Riot_model.Lockfile.t) ->
  List.fold_left
    (fun acc (pkg: Riot_model.Lockfile.package) ->
      match pkg.id.registry, pkg.id.version with
      | Some registry, Some version -> (registry ^ ":" ^ pkg.id.name, version) :: acc
      | _ -> acc)
    []
    lockfile.packages

let emit_updated_packages = fun ~(emit:event_sink) ~(previous:Riot_model.Lockfile.t) (
  current: Riot_model.Lockfile.t
) ->
  let previous_versions = lock_package_version_map previous in
  List.fold_left
    (fun updates (pkg: Riot_model.Lockfile.package) ->
      match pkg.id.registry, pkg.id.version with
      | Some registry, Some to_version -> (
          match List.assoc_opt (registry ^ ":" ^ pkg.id.name) previous_versions with
          | Some from_version when not (String.equal from_version to_version) ->
              emit
                (Riot_model.Event.PackageVersionUpdated {
                  package = pkg.id.name;
                  from_version;
                  to_version
                });
              updates + 1
          | _ -> updates
        )
      | _ -> updates)
    0
    current.packages

let update_manifest = fun ~(emit:event_sink) ~(target:target_manifest) ~scope ~dependencies ~operation ~dependency ->
  Manifest_edit.update_dependency_section
    ~manifest_path:target.path
    ~section:(scope_to_section scope)
    ~dependencies
  |> Result.map_error (fun error -> ManifestUpdateFailed { path = target.path; error })
  |> Result.map
    (fun () ->
      emit
        (Riot_model.Event.DependencyManifestUpdated {
          path = Path.to_string target.path;
          section = Manifest_edit.section_name (scope_to_section scope);
          operation;
          dependency
        }))

let add = fun ?(on_event = no_emit) ~(workspace:Riot_model.Workspace.t) ~cwd ~(request:add_request) () ->
  let emit = on_event in
  let* target = target_manifest ~workspace ~cwd request.selection request.scope in
  let* parsed =
    if is_source_dependency_spec request.dependency then
      load_source_dependency ~emit ~raw:request.dependency
    else
      parse_dependency_spec ~target request.dependency
  in
  let* parsed =
    match parsed with
    | Registry parsed ->
        let* registry = init_registry () in
        lookup_named_package ~emit ~registry parsed |> Result.map (fun parsed -> Registry parsed)
    | Path parsed ->
        Ok (Path parsed)
    | Source parsed ->
        Ok (Source parsed)
  in
  let dependencies = upsert_dependency target.dependencies (dependency_of_parsed parsed) in
  let* () =
    update_manifest ~emit ~target ~scope:request.scope ~dependencies ~operation:`Add
      ~dependency:((
        match parsed with
        | Registry parsed -> parsed.name
        | Path parsed -> parsed.name
        | Source parsed -> parsed.name
      ))
  in
  let* registry = init_registry () in
  let* workspace = reload_workspace ~workspace_root:workspace.root in
  let* _lockfile = refresh_lock ~emit ~mode:Dep_solver.Refresh ~registry ~workspace in
  Ok ()

let remove = fun ?(on_event = no_emit) ~(workspace:Riot_model.Workspace.t) ~cwd ~(request:remove_request) () ->
  let emit = on_event in
  let* registry = init_registry () in
  let* target = target_manifest ~workspace ~cwd request.selection request.scope in
  let dependencies, removed = remove_dependency target.dependencies ~name:request.dependency in
  if not removed then
    Error (DependencyNotFoundInSection {
      path = target.path;
      section = Manifest_edit.section_name (scope_to_section request.scope);
      dependency = request.dependency
    })
  else
    let* () = update_manifest
      ~emit
      ~target
      ~scope:request.scope
      ~dependencies
      ~operation:`Remove
      ~dependency:request.dependency in
    let* workspace = reload_workspace ~workspace_root:workspace.root in
    let* _lockfile = refresh_lock ~emit ~mode:Dep_solver.Refresh ~registry ~workspace in
    Ok ()

let update = fun ?(on_event = no_emit) ~(workspace:Riot_model.Workspace.t) () ->
  let emit = on_event in
  let* registry = init_registry () in
  let previous_lock =
    match Lockfile_store.read ~workspace_root:workspace.root with
    | Ok lockfile -> lockfile
    | Error _ -> None
  in
  let* lockfile = refresh_lock ~emit ~mode:Dep_solver.Unlock ~registry ~workspace in
  Option.iter
    (fun previous ->
      let updates = emit_updated_packages ~emit ~previous lockfile in
      if Int.equal updates 0 then
        emit (Riot_model.Event.PackageVersionsUnchanged { packages = List.length lockfile.packages }))
    previous_lock;
  Ok ()
