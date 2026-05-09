open Std

module ConcurrentHashMap = Collections.ConcurrentHashMap

type t = {
  workspace: Riot_model.Workspace.t;
  manifests_by_name: (Riot_model.Package_name.t, Riot_model.Package_manifest.t) ConcurrentHashMap.t;
  manifest_list: Riot_model.Package_manifest.t list;
  realized_packages:
    (Riot_model.Package_name.t * Riot_model.Package.realization_intent, Riot_model.Package.t) ConcurrentHashMap.t;
}

let create = fun (workspace: Riot_model.Workspace.t) ->
  let manifests = workspace.packages in
  let manifests_by_name = ConcurrentHashMap.with_capacity ~size:(List.length manifests) in
  List.for_each
    manifests
    ~fn:(fun (package: Riot_model.Package_manifest.t) ->
      ignore
        (ConcurrentHashMap.insert manifests_by_name ~key:package.name ~value:package));
  {
    workspace;
    manifests_by_name;
    manifest_list = manifests;
    realized_packages = ConcurrentHashMap.with_capacity ~size:(List.length manifests);
  }

let workspace = fun t -> t.workspace

let manifests = fun t -> t.manifest_list

let package_names = fun t ->
  List.map
    t.manifest_list
    ~fn:(fun (package: Riot_model.Package_manifest.t) -> package.name)

let find_manifest = fun t package_name ->
  ConcurrentHashMap.get
    t.manifests_by_name
    ~key:package_name

let require_manifest = fun t package_name ->
  match find_manifest t package_name with
  | Some package -> Ok package
  | None -> Error (Error.MissingPackage { package = package_name; available = package_names t })

let realize = fun t ~intent package_name ->
  let key = (package_name, intent) in
  match ConcurrentHashMap.get t.realized_packages ~key with
  | Some package -> Ok package
  | None ->
      require_manifest t package_name
      |> Result.map
        ~fn:(fun manifest ->
          let package = Riot_model.Workspace.realize_package ~intent t.workspace manifest in
          ignore (ConcurrentHashMap.insert t.realized_packages ~key ~value:package);
          package)
