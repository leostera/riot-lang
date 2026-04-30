open Std

(**
   Public entry points for the [riot check] command.

   Use this package when you need Riot's package-aware typechecking flow from
   a CLI wrapper, editor integration, or another automation layer.
*)

(** Command definition used by the CLI parser for [riot check]. *)
val command: ArgParser.command

(**
   Structured diagnostics produced by [riot check].

   Use this module when you want to render or post-process individual typing
   failures without reimplementing the checking flow.
*)
module Diagnostic: module type of Diagnostic

(** Checking orchestration and emitted events for [riot check]. *)
module Check: module type of Check

(** Typed errors returned when the check command cannot complete. *)
module Error: module type of Error

(**
   Run [riot check] using already-parsed CLI matches.

   [riot check] is always workspace-scoped. When no explicit paths are given,
   targets are resolved relative to [workspace], including package-aware
   filtering from ignore rules and optional package selection flags.

   Use [on_event] when you need streaming progress or structured JSON output.
   The callback receives the same event stream a CLI frontend can turn into
   human output.
*)
val run:
  workspace:Riot_model.Workspace.t ->
  ?on_event:(Check.Event.t -> unit) ->
  ArgParser.matches ->
  (unit, Error.t) result
