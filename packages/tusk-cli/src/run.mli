open Std

val command: Std.ArgParser.command

val build_scope_for_binary:
  Tusk_model.Workspace.t -> package_name:string -> binary_name:string -> Tusk_build.build_scope

val run: Std.ArgParser.matches -> (unit, exn) result
