open Std

(** Command definition for [riot check]. *)
val command: ArgParser.command

open Riot_model

(** Run [riot check] from already-parsed CLI matches.

    When [workspace] is provided, omitted targets are interpreted relative to that
    workspace context (package-aware ignore configuration and package roots). When
    [workspace] is omitted and no explicit targets are provided, [riot check]
    falls back to checking from the current directory.

    Optional [stdout] and [stderr] emitters allow tests and embedded callers to
    capture structured and human output without going through process-global
    stdio.
*)
val run:
  ?workspace:Workspace.t ->
  ?stdout:(string -> unit) ->
  ?stderr:(string -> unit) ->
  ArgParser.matches ->
  (unit, exn) result
