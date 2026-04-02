open Std

val command: Std.ArgParser.command

val run_check_paths:
  ?workspace:Riot_model.Workspace.t ->
  ?on_event:(Krasny.Report.event -> unit) ->
  Path.t list ->
  (unit, exn) result

val run:
  ?workspace:Riot_model.Workspace.t ->
  ?stdout:(string -> unit) ->
  ?stderr:(string -> unit) ->
  Std.ArgParser.matches ->
  (unit, exn) result
