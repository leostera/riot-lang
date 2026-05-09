open Std

module ConcurrentHashMap = Collections.ConcurrentHashMap

type t = {
  workspace: Riot_model.Workspace.t;
  packages_by_name: (Riot_model.Package_name.t, Riot_model.Package.t) ConcurrentHashMap.t;
  ordered_packages: Riot_model.Package.t list;
}

let create = fun (workspace: Riot_model.Workspace.t) ->
  let packages =
    Riot_model.Workspace.realize_packages ~intent:Riot_model.Package.Runtime workspace
    |> List.sort
      ~compare:(fun (left: Riot_model.Package.t) (right: Riot_model.Package.t) ->
        Riot_model.Package_name.compare
          left.name
          right.name)
  in
  let packages_by_name = ConcurrentHashMap.with_capacity ~size:(List.length packages) in
  List.for_each
    packages
    ~fn:(fun (package: Riot_model.Package.t) ->
      ignore
        (ConcurrentHashMap.insert packages_by_name ~key:package.name ~value:package));
  { workspace; packages_by_name; ordered_packages = packages }

let workspace = fun t -> t.workspace

let packages = fun t -> t.ordered_packages

let package_names = fun t ->
  List.map
    t.ordered_packages
    ~fn:(fun (package: Riot_model.Package.t) -> package.name)

let find = fun t package_name -> ConcurrentHashMap.get t.packages_by_name ~key:package_name

let require = fun t package_name ->
  match find t package_name with
  | Some package -> Ok package
  | None -> Error (Error.MissingPackage { package = package_name; available = package_names t })

let dependencies = fun t (package: Riot_model.Package.t) ->
  Riot_model.Package.dependencies_for_scope Riot_model.Package.Normal package
  |> List.filter_map
    ~fn:(fun (dependency: Riot_model.Package.dependency) ->
      if Riot_model.Package.is_builtin_dependency dependency then
        None
      else
        match find t dependency.name with
        | Some dependency_package -> Some dependency_package
        | None -> None)

let unsupported_external_dependencies = fun t (package: Riot_model.Package.t) ->
  Riot_model.Package.dependencies_for_scope Riot_model.Package.Normal package
  |> List.filter_map
    ~fn:(fun (dependency: Riot_model.Package.dependency) ->
      if Riot_model.Package.is_builtin_dependency dependency then
        None
      else
        match find t dependency.name with
        | Some _ -> None
        | None -> Some dependency.name)
