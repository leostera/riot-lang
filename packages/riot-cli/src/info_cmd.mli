open Std

val command: ArgParser.command

type workspace_scan =
  | NoWorkspace
  | ScanFailed of string
  | Loaded of Riot_model.Workspace_manifest.t * Riot_model.Workspace_manager.load_error list
type target =
  | Workspace_target
  | Package_target of string
val target_of_matches: ArgParser.matches -> target

val run: workspace_scan:workspace_scan -> ArgParser.matches -> (unit, exn) result
