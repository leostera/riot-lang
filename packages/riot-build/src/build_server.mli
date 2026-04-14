(** Build worker - Handles build execution in a spawned process. *)
open Riot_model

val start:
  workspace:Riot_model.Workspace.t ->
  load_errors:Riot_model.Workspace_manager.load_error list ->
  toolchain:Riot_toolchain.t ->
  concurrency:int ->
  session_id:Riot_model.Session_id.t ->
  client_pid:Std.Pid.t ->
  runtime_pid:Std.Pid.t ->
  target:Protocol.target ->
  scope:Protocol.build_scope ->
  profile:string ->
  target_arch:Riot_model.Target.t option ->
  unit

(** Start a build in a spawned worker process. This function returns immediately
    after spawning the worker. The worker will send results directly to the
    client_pid.

    The [concurrency] parameter is the single build concurrency budget and is
    propagated into [Build_ctx.available_parallelism] for action execution. *)
