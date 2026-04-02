open Std

val command: Std.ArgParser.command

val build_scope_for_binary:
  Riot_model.Workspace.t -> package_name:string -> binary_name:string -> Riot_build.build_scope

val run: workspace:Riot_model.Workspace.t -> Std.ArgParser.matches -> (unit, exn) result
