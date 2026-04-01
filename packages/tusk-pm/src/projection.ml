open Std

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

let find_lock_package_by_id = fun ~(package_id: Tusk_model.Lockfile.package_id) ~(lockfile: Tusk_model.Lockfile.t) ->
  List.find_opt
    (fun (lock_package: Tusk_model.Lockfile.package) ->
      lock_package.id = package_id)
    lockfile.packages

let find_workspace_package_by_id = fun ~(package_id: Tusk_model.Lockfile.package_id) ~(packages: Tusk_model.Package.t list) ->
  List.find_opt
    (fun (package: Tusk_model.Package.t) ->
      workspace_package_id_of_package package = package_id)
    packages

let load_manifest_toml = fun ~manifest_path ->
  match Fs.read manifest_path with
  | Error err ->
      Error ("failed to read manifest '" ^ Path.to_string manifest_path ^ "': " ^ IO.error_message err)
  | Ok source -> (
      match Data.Toml.parse source with
      | Ok toml -> Ok toml
      | Error err ->
          Error ("failed to parse manifest TOML '" ^ Path.to_string manifest_path ^ "': " ^ Data.Toml.error_to_string err)
    )

let load_external_package = fun ~(lock_package: Tusk_model.Lockfile.package) ->
  match load_manifest_toml ~manifest_path:lock_package.manifest_path with
  | Error _ as err -> err
  | Ok toml ->
      let package_root = Path.dirname lock_package.manifest_path in
      Tusk_model.Package.from_toml
        toml
        ~workspace_deps:[]
        ~workspace_dev_deps:[]
        ~workspace_build_deps:[]
        ~path:package_root
        ~relative_path:package_root

let load_package_for_lock_package = fun ~(packages: Tusk_model.Package.t list) ~(lock_package: Tusk_model.Lockfile.package) ->
  match lock_package.provenance with
  | Tusk_model.Lockfile.Workspace -> (
      match find_workspace_package_by_id ~package_id:lock_package.id ~packages with
      | Some package -> Ok package
      | None -> Error ("workspace package '"
      ^ lock_package.id.name
      ^ "' was not provided to projection")
    )
  | Tusk_model.Lockfile.Path _
  | Tusk_model.Lockfile.Registry _ -> load_external_package ~lock_package

let resolve_dependency_ids = fun (resolved: Tusk_model.Package.resolved) ->
  List.map (fun (dep: Tusk_model.Package.resolved_dependency) -> dep.resolved_id) resolved.runtime_resolved
  @ List.map (fun (dep: Tusk_model.Package.resolved_dependency) -> dep.resolved_id) resolved.build_resolved
  @ List.map (fun (dep: Tusk_model.Package.resolved_dependency) -> dep.resolved_id) resolved.dev_resolved

let rec resolve_package_graph = fun ~(packages: Tusk_model.Package.t list) ~(lockfile: Tusk_model.Lockfile.t) seen acc pending ->
  match pending with
  | [] -> Ok (List.rev acc)
  | package_id :: rest ->
      let key = package_id_key package_id in
      if List.mem key seen then
        resolve_package_graph ~packages ~lockfile seen acc rest
      else
        match find_lock_package_by_id ~package_id ~lockfile with
        | None ->
            Error ("lockfile is missing package '"
            ^ package_id.name
            ^ "'")
        | Some lock_package -> (
            match load_package_for_lock_package ~packages ~lock_package with
            | Error _ as err -> err
            | Ok package -> (
                match Tusk_model.Package.resolve ~package ~lock_package with
                | Error _ as err -> err
                | Ok resolved ->
                    let dependency_ids = resolve_dependency_ids resolved in
                    resolve_package_graph
                      ~packages
                      ~lockfile
                      (key :: seen)
                      (resolved :: acc)
                      (dependency_ids @ rest)
              )
          )

let resolve_packages = fun ~packages ~lockfile ->
  let root_ids =
    List.map workspace_package_id_of_package packages
  in
  resolve_package_graph ~packages ~lockfile [] [] root_ids
