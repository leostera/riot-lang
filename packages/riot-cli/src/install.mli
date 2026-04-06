open Std

val command: Std.ArgParser.command

val run_with_workspace_info:
  workspace:Riot_model.Workspace.t option ->
  workspace_error:string option ->
  Std.ArgParser.matches ->
  (unit, exn) result

val run: workspace:Riot_model.Workspace.t -> Std.ArgParser.matches -> (unit, exn) result
