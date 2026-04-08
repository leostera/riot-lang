open Std

val command: ArgParser.command

type workspace_scan =
  | NoWorkspace
  | ScanFailed of string
  | Loaded of Riot_model.Workspace.t * Riot_model.Workspace_manager.load_error list
val run: workspace_scan:workspace_scan -> ArgParser.matches -> (unit, exn) result
