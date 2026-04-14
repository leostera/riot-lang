open Std

type t

val of_workspace:
  ?workspace_manager:Riot_model.Workspace_manager.t ->
  Riot_model.Workspace.t ->
  t

val workspace: t -> Riot_model.Workspace.t

val workspace_manager: t -> Riot_model.Workspace_manager.t option

val package_names: t -> string list
