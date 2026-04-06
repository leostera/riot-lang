open Std

val command: ArgParser.command

open Riot_model

val run:
  ?workspace:Workspace.t ->
  ?stdout:(string -> unit) ->
  ?stderr:(string -> unit) ->
  ArgParser.matches ->
  (unit, exn) result

val populate_workspace_typings: workspace:Workspace.t -> package_names:string list -> unit -> unit
