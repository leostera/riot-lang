open Std

let ( let* ) = Result.and_then

type dependency_scope =
  | Runtime
  | Build
  | Dev

type manifest_selection =
  | Current
  | Workspace
  | Package of string

type event =
  | RegistryPackageLookupStarted of { package: string }
  | RegistryPackageLookupFinished of { package: string; latest_version: string }
  | ManifestUpdated of { path: Path.t; section: string; operation:
        [
          `Add
          | `Remove
        ]; dependency: string }
  | Pm of Tusk_model.Event.kind

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
  | RegistryInitializationFailed of { registry: string; error: string }
  | RegistryLookupFailed of { package: string; registry: string; error: string }
  | RegistryPackageNotFound of { package: string; registry: string }
  | RegistryVersionNotFound of { package: string; requirement: string; registry: string }
  | ManifestUpdateFailed of { path: Path.t; error: string }
  | DependencyNotFoundInSection of { path: Path.t; section: string; dependency: string }
  | WorkspaceReloadFailed of { workspace_root: Path.t; error: string }
  | WorkspaceReloadHadErrors of { workspace_root: Path.t; errors: string list }
  | LockRefreshFailed of Error.t

type target_manifest = {
  path: Path.t;
  dependencies: Tusk_model.Package.dependency list;
}

type parsed_dependency = {
  name: string;
  requirement: Std.Version.requirement option;
}

let no_emit = fun _ -> ()

let registry_name = "pkgs.ml"

let error_message = function
  | CurrentPackageNotFound { cwd } -> "could not determine current package from '"
  ^ Path.to_string cwd
  ^ "'"
  | PackageNotFound { package } -> "workspace package '" ^ package ^ "' was not found"
  | DependencySpecInvalid { dependency; error } -> "invalid dependency '" ^ dependency ^ "': " ^ error
  | RegistryInitializationFailed { registry; error } -> "failed to initialize registry '"
  ^ registry
  ^ "': "
  ^ error
  | RegistryLookupFailed { package; registry; error } -> "failed to look up package '"
  ^ package
  ^ "' in registry '"
  ^ registry
  ^ "': "
  ^ error
  | RegistryPackageNotFound { package; registry } -> "package '"
  ^ package
  ^ "' was not found in registry '"
  ^ registry
  ^ "'"
  | RegistryVersionNotFound { package; requirement; registry } -> "package '"
  ^ package
  ^ "' has no release matching '"
  ^ requirement
  ^ "' in registry '"
  ^ registry
  ^ "'"
  | ManifestUpdateFailed { path; error } -> "failed to update manifest '"
  ^ Path.to_string path
  ^ "': "
  ^ error
  | DependencyNotFoundInSection { path; section; dependency } -> "dependency '"
  ^ dependency
  ^ "' was not found in ["
  ^ section
  ^ "] of '"
  ^ Path.to_string path
  ^ "'"
  | WorkspaceReloadFailed { workspace_root; error } -> "failed to reload workspace '"
  ^ Path.to_string workspace_root
  ^ "': "
  ^ error
  | WorkspaceReloadHadErrors { workspace_root; errors } -> "workspace '"
  ^ Path.to_string workspace_root
  ^ "' has load errors:\n"
  ^ String.concat "\n" errors
  | LockRefreshFailed error -> Tusk_model.Pm_error.message error

let scope_to_section = function
  | Runtime -> Manifest_edit.Runtime
  | Build -> Manifest_edit.Build
  | Dev -> Manifest_edit.Dev

let parse_dependency_spec = fun raw ->
  match String.split_on_char '@' raw with
  | [ name ] ->
      let* name = Tusk_model.Package.validate_name (String.trim name)
      |> Result.map_error (fun error -> DependencySpecInvalid { dependency = raw; error }) in
      Ok { name; requirement = Some Std.Version.any }
  | [name;requirement] ->
      let* name = Tusk_model.Package.validate_name (String.trim name)
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

let init_registry = fun () ->
  Pkgs_ml.Registry.create_filesystem ~registry_name ()
  |> Result.map_error (fun error -> RegistryInitializationFailed { registry = registry_name; error })

let dependency_lists_for_package = fun scope (pkg: Tusk_model.Package.t) ->
  match scope with
  | Runtime -> pkg.dependencies
  | Build -> pkg.build_dependencies
  | Dev -> pkg.dev_dependencies

let dependency_lists_for_workspace = fun scope (workspace: Tusk_model.Workspace.t) ->
  match scope with
  | Runtime -> workspace.dependencies
  | Build -> workspace.build_dependencies
  | Dev -> workspace.dev_dependencies

let select_current_package = fun ~(workspace:Tusk_model.Workspace.t) ~cwd ->
  match Tusk_model.Workspace.find_package_for_path workspace ~path:cwd with
  | Some pkg -> Ok pkg
  | None -> (
      match List.filter Tusk_model.Package.is_workspace_member workspace.packages with
      | [ pkg ] -> Ok pkg
      | _ -> Error (CurrentPackageNotFound { cwd })
    )

let target_manifest = fun ~(workspace:Tusk_model.Workspace.t) ~cwd selection scope ->
  match selection with
  | Workspace ->
      Ok {
        path =
          Path.(workspace.root / Path.v "tusk.toml");
        dependencies = dependency_lists_for_workspace scope workspace;
      }
  | Package package_name -> (
      match
        workspace.packages |> List.filter Tusk_model.Package.is_workspace_member |> List.find_opt
          (fun (pkg: Tusk_model.Package.t) ->
            String.equal pkg.name package_name)
      with
      | Some pkg ->
          Ok {
            path =
              Path.(pkg.path / Path.v "tusk.toml");
            dependencies = dependency_lists_for_package scope pkg;
          }
      | None -> Error (PackageNotFound { package = package_name })
    )
  | Current ->
      let* pkg = select_current_package ~workspace ~cwd in
      Ok {
        path =
          Path.(pkg.path / Path.v "tusk.toml");
        dependencies = dependency_lists_for_package scope pkg;
      }

let dependency_exists = fun ~(package_name:string) document requirement ->
  let rec loop = function
    | [] -> false
    | release :: rest -> (
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

let lookup_named_package = fun ~(emit:event -> unit) ~registry parsed ->
  emit (RegistryPackageLookupStarted { package = parsed.name });
  let* document = Pkgs_ml.Registry.read_package_document registry ~package_name:parsed.name
  |> Result.map_error
    (fun error -> RegistryLookupFailed { package = parsed.name; registry = registry_name; error }) in
  match document with
  | None -> Error (RegistryPackageNotFound { package = parsed.name; registry = registry_name })
  | Some document ->
      emit
        (RegistryPackageLookupFinished { package = document.name; latest_version = document.latest });
      let requirement = Option.unwrap_or ~default:Std.Version.any parsed.requirement in
      if dependency_exists ~package_name:parsed.name document requirement then
        Ok parsed
      else
        Error (RegistryVersionNotFound {
          package = parsed.name;
          requirement = Std.Version.requirement_to_string requirement;
          registry = registry_name
        })

let upsert_dependency = fun (dependencies: Tusk_model.Package.dependency list) ((
  dependency: Tusk_model.Package.dependency
) as replacement) ->
  let rec loop (acc: Tusk_model.Package.dependency list) (
    remaining: Tusk_model.Package.dependency list
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
    List.filter (fun (dep: Tusk_model.Package.dependency) -> not (String.equal dep.name name)) dependencies
  in
  (kept, not (Int.equal (List.length kept) (List.length dependencies)))

let dependency_of_parsed = fun (parsed: parsed_dependency) ->
  Tusk_model.Package.{
    name = parsed.name;
    source = {
      workspace = false;
      builtin = Tusk_model.Package.is_builtin_dependency_name parsed.name;
      path = None;
      version = parsed.requirement
    }
  }

let reload_workspace = fun ~(workspace_root:Path.t) ->
  let* (workspace, load_errors) = Tusk_model.Workspace_manager.scan workspace_root
  |> Result.map_error (fun error -> WorkspaceReloadFailed { workspace_root; error }) in
  match load_errors with
  | [] -> Ok workspace
  | load_errors ->
      let errors = List.map Tusk_model.Workspace_manager.load_error_to_string load_errors in
      Error (WorkspaceReloadHadErrors { workspace_root; errors })

let refresh_lock = fun ~(emit:event -> unit) ~mode ~registry ~(workspace:Tusk_model.Workspace.t) ->
  Workspace_resolution.ensure_lock ~emit:(fun event -> emit (Pm event)) ~mode ~registry ~workspace ()
  |> Result.map ignore
  |> Result.map_error (fun error -> LockRefreshFailed error)

let update_manifest = fun ~(emit:event -> unit) ~target ~scope ~dependencies ~operation ~dependency ->
  Manifest_edit.update_dependency_section
    ~manifest_path:target.path
    ~section:(scope_to_section scope)
    ~dependencies
  |> Result.map_error (fun error -> ManifestUpdateFailed { path = target.path; error })
  |> Result.map
    (fun () ->
      emit
        (ManifestUpdated {
          path = target.path;
          section = Manifest_edit.section_name (scope_to_section scope);
          operation;
          dependency
        }))

let add = fun ?(on_event = no_emit) ~(workspace:Tusk_model.Workspace.t) ~cwd ~(request:add_request) () ->
  let emit = on_event in
  let* registry = init_registry () in
  let* parsed = parse_dependency_spec request.dependency in
  let* parsed = lookup_named_package ~emit ~registry parsed in
  let* target = target_manifest ~workspace ~cwd request.selection request.scope in
  let dependencies = upsert_dependency target.dependencies (dependency_of_parsed parsed) in
  let* () = update_manifest
    ~emit
    ~target
    ~scope:request.scope
    ~dependencies
    ~operation:`Add
    ~dependency:parsed.name in
  let* workspace = reload_workspace ~workspace_root:workspace.root in
  refresh_lock ~emit ~mode:Dep_solver.Refresh ~registry ~workspace

let remove = fun ?(on_event = no_emit) ~(workspace:Tusk_model.Workspace.t) ~cwd ~(request:remove_request) () ->
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
    refresh_lock ~emit ~mode:Dep_solver.Refresh ~registry ~workspace

let update = fun ?(on_event = no_emit) ~(workspace:Tusk_model.Workspace.t) () ->
  let emit = on_event in
  let* registry = init_registry () in
  refresh_lock ~emit ~mode:Dep_solver.Unlock ~registry ~workspace
