open Std

val command: ArgParser.command

open Riot_model

val run:
  workspace:Workspace.t ->
  ?stdout:(string -> unit) ->
  ?stderr:(string -> unit) ->
  ArgParser.matches ->
  (unit, exn) result
