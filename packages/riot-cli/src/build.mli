open Std

(** CLI support for [riot build].

    This module owns user-facing build output and delegates the actual build
    execution to [riot-build].
*)
type build_scope = Riot_build.build_scope =
  | Runtime
  | Dev
type output_mode =
  | Human
  | Json
type build_progress = {
  (** Number of packages built in this render pass. *)
  mutable built_count: int;
  (** Number of cache hits reported so far. *)
  mutable cached_count: int;
  (** Number of failed packages reported so far. *)
  mutable failed_count: int;
  (** Number of skipped packages reported so far. *)
  mutable skipped_count: int;
}

(** Command definition for [riot build]. *)
val command: Std.ArgParser.command

(** Format a package-manager event for human output.

    Returns [None] when the event should stay silent in human mode.
*)
val format_pm_event:
  seen_registry_updates:string Std.Collections.HashSet.t -> Riot_model.Event.kind -> string option

(** Reset the monotonic clock used for emitted JSON build events. *)
val reset_json_clock: started_at:Std.Time.Instant.t -> unit

(** Emit a build event as JSON. *)
val write_build_event_json: Riot_build.build_event -> unit

(** Render a package-manager event in the selected output mode. *)
val write_pm_event:
  mode:output_mode ->
  seen_registry_updates:string Std.Collections.HashSet.t ->
  Riot_model.Event.t ->
  unit

(** Render a "building target" status event. *)
val write_building_target_event: mode:output_mode -> target:string -> host:bool -> unit

(** Render a cache-GC event produced during the build flow. *)
val write_cache_gc_event: mode:output_mode -> Riot_store.Cache_gc.event -> unit

(** Render a streamed build-runtime event and update progress counters. *)
val write_streaming_event:
  mode:output_mode ->
  displayed_packages:string Std.Collections.HashSet.t ->
  progress:build_progress ->
  Riot_build.Client.streaming_event ->
  unit

(** Run [riot build] in a resolved workspace. *)
val run: workspace:Riot_model.Workspace.t -> Std.ArgParser.matches -> (unit, exn) result

(** Execute a build command programmatically.

    Use this entry point when another CLI command wants the same build surface
    with explicit control over scope, profile, output mode, or prepared state.
*)
val build_command:
  ?workspace:Riot_model.Workspace.t ->
  ?prepared:bool ->
  ?scope:build_scope ->
  ?profile:string ->
  ?mode:output_mode ->
  ?show_finished_summary:bool ->
  string option ->
  string option ->
  (unit, exn) result
