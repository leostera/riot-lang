open Std

type error =
  | ScanFailed of Riot_model.Workspace_manager.scan_error
  | LoadErrors of Riot_model.Workspace_manager.load_error list

val error_message: error -> string

val load_local: root:Path.t -> (Riot_model.Workspace.t, error) result
