open Std

type mode =
  | Refresh
  | Unlock

type resolved_dependency = {
  dependency: Tusk_model.Lockfile.dependency;
  packages: Tusk_model.Lockfile.package list;
}

let package_id_of_workspace_package = fun (pkg: Tusk_model.Package.t) ->
  Tusk_model.Lockfile.{ registry = None; name = pkg.name; version = None }

let manifest_path_for_package = fun (pkg: Tusk_model.Package.t) ->
  Path.(pkg.path / Path.v "tusk.toml")

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

let materialized_root_for_registry_package = fun ~registry_cache ~package_name ~version ->
  Pkgs_ml.Registry_cache.package_src_dir registry_cache ~package_name ~version

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

let lock_dependency_of_workspace_dependency = fun (dep: Tusk_model.Package.dependency) ->
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

let rec resolve_registry_dependency = fun ~mode ~registry ~registry_cache ~registry_name ~existing_lock package_name ->
  match mode, find_existing_external_package ~registry_name ~existing_lock ~package_name with
  | Refresh, Some (existing_pkg: Tusk_model.Lockfile.package) ->
      Ok {
        dependency = Tusk_model.Lockfile.{ name = package_name; package = existing_pkg.id };
        packages = [];
      }
  | _ ->
      match Pkgs_ml.Registry.read_package_document registry ~package_name with
      | Error err ->
          Error ("failed to read package document for '" ^ package_name ^ "': " ^ err)
      | Ok None ->
          Error ("package '" ^ package_name ^ "' was not found in registry '" ^ registry_name ^ "'")
      | Ok (Some document) -> (
          match latest_release_of_document document with
          | Error _ as err -> err
          | Ok (release: Pkgs_ml.Sparse_index.release) ->
              let rec resolve_release_dependencies
                (acc_packages: Tusk_model.Lockfile.package list)
                (acc_dependencies: Tusk_model.Lockfile.dependency list)
                (release_dependencies: Pkgs_ml.Sparse_index.dependency list)
              =
                match release_dependencies with
                | [] -> Ok (List.rev acc_dependencies, acc_packages)
                | (dep: Pkgs_ml.Sparse_index.dependency) :: rest -> (
                    match
                      resolve_registry_dependency
                        ~mode
                        ~registry
                        ~registry_cache
                        ~registry_name
                        ~existing_lock
                        dep.name
                    with
                    | Error _ as err -> err
                    | Ok resolved ->
                        resolve_release_dependencies
                          (List.rev_append resolved.packages acc_packages)
                          (resolved.dependency :: acc_dependencies)
                          rest
                  )
              in
              match resolve_release_dependencies [] [] release.dependencies with
              | Error _ as err -> err
              | Ok (dependencies, dependency_packages) ->
                  let path =
                    materialized_root_for_registry_package
                      ~registry_cache
                      ~package_name:document.name
                      ~version:release.version
                  in
                  let lock_package =
                    Tusk_model.Lockfile.{
                      id = {
                        registry = Some registry_name;
                        name = document.name;
                        version = Some release.version;
                      };
                      path;
                      manifest_path = manifest_path_for_materialized_root path;
                      provenance = Registry { registry = registry_name };
                      dependencies;
                      build_dependencies = [];
                      dev_dependencies = [];
                    }
                  in
                  Ok {
                    dependency = Tusk_model.Lockfile.{ name = package_name; package = lock_package.id };
                    packages = dependency_packages @ [ lock_package ];
                  }
        )

let rec resolve_manifest_dependencies = fun ~mode ~registry ~registry_cache ~registry_name ~existing_lock acc_packages acc_dependencies deps ->
  match deps with
  | [] -> Ok (List.rev acc_dependencies, List.rev acc_packages)
  | dep :: rest -> (
      match dep.Tusk_model.Package.source with
      | Tusk_model.Package.Workspace ->
          resolve_manifest_dependencies
            ~mode
            ~registry
            ~registry_cache
            ~registry_name
            ~existing_lock
            acc_packages
            (lock_dependency_of_workspace_dependency dep :: acc_dependencies)
            rest
      | Tusk_model.Package.Registry _ -> (
          match
            resolve_registry_dependency
              ~mode
              ~registry
              ~registry_cache
              ~registry_name
              ~existing_lock
              dep.name
          with
          | Error _ as err -> err
          | Ok resolved ->
              resolve_manifest_dependencies
                ~mode
                ~registry
                ~registry_cache
                ~registry_name
                ~existing_lock
                (List.rev_append resolved.packages acc_packages)
                (resolved.dependency :: acc_dependencies)
                rest
        )
      | Tusk_model.Package.Path path ->
          Error ("path dependencies are not implemented in tusk-pm yet: '"
          ^ dep.name
          ^ "' -> "
          ^ Path.to_string path)
    )

let lock_package_of_workspace_package = fun ~mode ~registry ~registry_cache ~registry_name ~existing_lock (pkg: Tusk_model.Package.t) ->
  match
    resolve_manifest_dependencies
      ~mode
      ~registry
      ~registry_cache
      ~registry_name
      ~existing_lock
      []
      []
      pkg.dependencies
  with
  | Error _ as err -> err
  | Ok (dependencies, dependency_packages) -> (
      match
        resolve_manifest_dependencies
          ~mode
          ~registry
          ~registry_cache
          ~registry_name
          ~existing_lock
          []
          []
          pkg.build_dependencies
      with
      | Error _ as err -> err
      | Ok (build_dependencies, build_packages) -> (
          match
            resolve_manifest_dependencies
              ~mode
              ~registry
              ~registry_cache
              ~registry_name
              ~existing_lock
              []
              []
              pkg.dev_dependencies
          with
          | Error _ as err -> err
          | Ok (dev_dependencies, dev_packages) ->
              Ok
                ( Tusk_model.Lockfile.{
                    id = package_id_of_workspace_package pkg;
                    path = pkg.path;
                    manifest_path = manifest_path_for_package pkg;
                    provenance = Workspace;
                    dependencies;
                    build_dependencies;
                    dev_dependencies;
                  },
                  dependency_packages @ build_packages @ dev_packages )
        )
    )

let rec lock_packages = fun ~mode ~registry ~registry_cache ~registry_name ~existing_lock acc_workspace acc_external packages ->
  match packages with
  | [] -> Ok (List.rev acc_workspace, List.rev acc_external)
  | pkg :: rest -> (
      match
        lock_package_of_workspace_package
          ~mode
          ~registry
          ~registry_cache
          ~registry_name
          ~existing_lock
          pkg
      with
      | Ok (pkg, external_packages) ->
          lock_packages
            ~mode
            ~registry
            ~registry_cache
            ~registry_name
            ~existing_lock
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

let lock_deps = fun ~mode ~registry ~registry_cache ~registry_name ~existing_lock packages ->
  match
    lock_packages
      ~mode
      ~registry
      ~registry_cache
      ~registry_name
      ~existing_lock
      []
      []
      packages
  with
  | Ok (workspace_packages, external_packages) ->
      let preserved =
        match (mode, existing_lock) with
        | Unlock, _ -> []
        | Refresh, Some (existing_lock: Tusk_model.Lockfile.t) ->
            List.filter (keep_existing_package packages) existing_lock.packages
        | Refresh, None -> []
      in
      let packages = merge_lock_packages (workspace_packages @ external_packages @ preserved) in
      Ok Tusk_model.Lockfile.{ format_version = 1; packages }
  | Error _ as err -> err
