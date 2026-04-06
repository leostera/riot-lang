open Std

val command: Std.ArgParser.command

val build_scope_for_binary:
  Riot_model.Workspace.t -> package_name:string -> binary_name:string -> Riot_build.build_scope

val default_remote_binary_name: string -> string

type implicit_local_target = {
  package_name: string;
  binary_name: string;
}
val resolve_implicit_local_target:
  ?package_filter:string -> Riot_model.Workspace.t -> (implicit_local_target, string) result

val run_with_workspace_info:
  workspace:Riot_model.Workspace.t option ->
  workspace_error:string option ->
  Std.ArgParser.matches ->
  (unit, exn) result

val run: workspace:Riot_model.Workspace.t -> Std.ArgParser.matches -> (unit, exn) result
