open Std

type mode =
  | Refresh
  | Unlock

let package_id_of_workspace_package = fun (pkg: Tusk_model.Package.t) ->
  Tusk_model.Lockfile.{ registry = None; name = pkg.name; version = None }

let manifest_path_for_package = fun (pkg: Tusk_model.Package.t) ->
  Path.(pkg.path / Path.v "tusk.toml")

let lock_dependency_of_manifest_dependency = fun ~registry_name (dep: Tusk_model.Package.dependency) ->
  match dep.source with
  | Tusk_model.Package.Workspace ->
      Ok Tusk_model.Lockfile.{
        name = dep.name;
        package = { registry = None; name = dep.name; version = None };
      }
  | Tusk_model.Package.Registry _ ->
      Ok Tusk_model.Lockfile.{
        name = dep.name;
        package = { registry = Some registry_name; name = dep.name; version = None };
      }
  | Tusk_model.Package.Path path ->
      Error
        ("path dependencies are not implemented in tusk-pm yet: '"
        ^ dep.name
        ^ "' -> "
        ^ Path.to_string path)

let rec lock_dependencies_of_manifest_dependencies = fun ~registry_name acc deps ->
  match deps with
  | [] -> Ok (List.rev acc)
  | dep :: rest -> (
      match lock_dependency_of_manifest_dependency ~registry_name dep with
      | Ok dep -> lock_dependencies_of_manifest_dependencies ~registry_name (dep :: acc) rest
      | Error _ as err -> err
    )

let lock_package_of_workspace_package = fun ~registry_name (pkg: Tusk_model.Package.t) ->
  match lock_dependencies_of_manifest_dependencies ~registry_name [] pkg.dependencies with
  | Error _ as err -> err
  | Ok dependencies -> (
      match lock_dependencies_of_manifest_dependencies ~registry_name [] pkg.build_dependencies with
      | Error _ as err -> err
      | Ok build_dependencies -> (
          match lock_dependencies_of_manifest_dependencies ~registry_name [] pkg.dev_dependencies with
          | Error _ as err -> err
          | Ok dev_dependencies ->
              Ok Tusk_model.Lockfile.{
                id = package_id_of_workspace_package pkg;
                path = pkg.path;
                manifest_path = manifest_path_for_package pkg;
                provenance = Workspace;
                dependencies;
                build_dependencies;
                dev_dependencies;
              }))

let rec lock_packages = fun ~registry_name acc packages ->
  match packages with
  | [] -> Ok (List.rev acc)
  | pkg :: rest -> (
      match lock_package_of_workspace_package ~registry_name pkg with
      | Ok pkg -> lock_packages ~registry_name (pkg :: acc) rest
      | Error _ as err -> err
    )

let keep_existing_package = fun workspace_packages (pkg: Tusk_model.Lockfile.package) ->
  let workspace_names = List.map (fun (pkg: Tusk_model.Package.t) -> pkg.name) workspace_packages in
  not (List.mem pkg.id.name workspace_names)

let lock_deps = fun ~mode ~registry_name ~existing_lock packages ->
  match lock_packages ~registry_name [] packages with
  | Ok workspace_packages ->
      let packages =
        match (mode, existing_lock) with
        | Unlock, _ ->
            workspace_packages
        | Refresh, Some (existing_lock: Tusk_model.Lockfile.t) ->
            let preserved =
              List.filter
                (keep_existing_package packages)
                existing_lock.packages
            in
            workspace_packages @ preserved
        | Refresh, None ->
            workspace_packages
      in
      Ok Tusk_model.Lockfile.{ format_version = 1; packages }
  | Error _ as err -> err
