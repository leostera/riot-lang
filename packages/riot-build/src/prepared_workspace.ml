open Std

type t = {
  workspace: Riot_model.Workspace.t;
  workspace_manager: Riot_model.Workspace_manager.t option;
}

let of_workspace = fun ?workspace_manager workspace ->
  { workspace; workspace_manager }

module Internal = struct
  let workspace = fun t -> t.workspace

  let workspace_manager = fun t -> t.workspace_manager

  let package_names = fun t ->
    t.workspace.packages
    |> List.map ~fn:(fun (pkg: Riot_model.Package_manifest.t) -> pkg.name)
end
