open Std
open Std.Collections
open Riot_model
open Riot_planner

type workspace_result = {
  results: Package_builder.build_result list;
  total_duration: Time.Duration.t;
  cached_count: int;
  built_count: int;
  failed_count: int;
  package_graph: Package_graph.t;
}
val build_workspace:
  workspace:Workspace.t ->
  toolchain:Riot_toolchain.t ->
  store:Riot_store.Store.t ->
  target:Workspace_planner.target ->
  scope:Package_graph.build_scope ->
  build_ctx:Build_ctx.t ->
  session_id:Session_id.t ->
  (workspace_result, Workspace_planner.plan_error) result

(** Plan and execute a workspace build.

    Scheduler concurrency is sourced from `build_ctx.parallelism`; package
    orchestration must not create additional competing worker pools. *)
