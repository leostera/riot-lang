open Std

type build_scope =
  Runtime
  | Dev
type output_mode =
  | Human
  | Json
val command: Std.ArgParser.command

val format_pm_event:
  seen_registry_updates:string Std.Collections.HashSet.t ->
  Tusk_model.Event.kind ->
  string option

val run:
  workspace:Tusk_model.Workspace.t ->
  load_errors:Tusk_model.Workspace_manager.load_error list ->
  Std.ArgParser.matches ->
  (unit, exn) result

val build_command:
  ?workspace:Tusk_model.Workspace.t ->
  ?load_errors:Tusk_model.Workspace_manager.load_error list ->
  ?scope:build_scope ->
  ?mode:output_mode ->
  ?show_finished_summary:bool ->
  string option ->
  string option ->
  (unit, exn) result
