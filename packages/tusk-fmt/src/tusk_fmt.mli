open Std

val command: Std.ArgParser.command

val run_check_paths:
  ?workspace:Tusk_model.Workspace.t ->
  ?on_event:(Krasny.Report.event -> unit) ->
  Path.t list ->
  (unit, exn) result

val run: ?workspace:Tusk_model.Workspace.t -> Std.ArgParser.matches -> (unit, exn) result
