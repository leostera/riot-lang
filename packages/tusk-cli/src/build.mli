open Std

type build_scope = Tusk_build.build_scope =
  Runtime
  | Dev
type output_mode =
  | Human
  | Json
type build_progress = {
  mutable built_count: int;
  mutable cached_count: int;
  mutable failed_count: int;
  mutable skipped_count: int;
}
val command: Std.ArgParser.command

val format_pm_event:
  seen_registry_updates:string Std.Collections.HashSet.t -> Tusk_model.Event.kind -> string option

val write_pm_event:
  mode:output_mode ->
  seen_registry_updates:string Std.Collections.HashSet.t ->
  Tusk_model.Event.t ->
  unit

val write_building_target_event: mode:output_mode -> target:string -> host:bool -> unit

val write_streaming_event:
  mode:output_mode ->
  displayed_packages:string Std.Collections.HashSet.t ->
  progress:build_progress ->
  Tusk_build.Client.streaming_event ->
  unit

val run: workspace:Tusk_model.Workspace.t -> Std.ArgParser.matches -> (unit, exn) result

val build_command:
  ?workspace:Tusk_model.Workspace.t ->
  ?scope:build_scope ->
  ?profile:string ->
  ?mode:output_mode ->
  ?show_finished_summary:bool ->
  string option ->
  string option ->
  (unit, exn) result
