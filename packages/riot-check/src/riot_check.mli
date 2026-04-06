open Std

(** Command definition for [riot check]. *)
val command: ArgParser.command

module Diagnostic: module type of Diagnostic

module Check: module type of Check

module Error: module type of Error

(** Run [riot check] from already-parsed CLI matches.

    [riot check] is workspace-scoped. Omitted targets are interpreted relative
    to the provided workspace context, including package-aware ignore
    configuration and optional [-p]/[--package] narrowing. Structured command
    events are emitted through [on_event]; callers own all final rendering.
*)
val run:
  workspace:Riot_model.Workspace.t ->
  ?on_event:(Check.Event.t -> unit) ->
  ArgParser.matches ->
  (unit, Error.t) result
