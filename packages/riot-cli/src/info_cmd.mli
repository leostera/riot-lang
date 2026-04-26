open Std

val command: ArgParser.command

type workspace_scan =
  | NoWorkspace
  | ScanFailed of workspace_scan_error
  | Loaded of Riot_model.Workspace_manifest.t * Riot_model.Workspace_manager.load_error list

and workspace_scan_error =
  | CurrentDirReadFailed of Path.error
  | WorkspaceScanFailed of Riot_model.Workspace_manager.scan_error
type target =
  | Workspace_target
  | Package_target of string
val target_of_matches: ArgParser.matches -> target

val workspace_scan_error_message: workspace_scan_error -> string

val run: workspace_scan:workspace_scan -> ArgParser.matches -> (unit, exn) result
