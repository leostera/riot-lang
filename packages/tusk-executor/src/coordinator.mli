open Std
open Std.Collections
open Tusk_model
open Tusk_planner

type workspace_result = {
  results : Package_builder.build_result list;
  total_duration : Time.Duration.t;
  cached_count : int;
  built_count : int;
  failed_count : int;
  package_graph : Package_graph.t;
}

val build_workspace :
  workspace:Workspace.t ->
  toolchain:Tusk_toolchain.t ->
  store:Tusk_store.Store.t ->
  target:Workspace_planner.target ->
  scope:Package_graph.build_scope ->
  concurrency:int ->
  build_ctx:Build_ctx.t ->
  session_id:Session_id.t ->
  (workspace_result, Workspace_planner.plan_error) result
