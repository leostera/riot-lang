open Std
open Std.Collections
open Tusk_model
open Tusk_planner

(** Result of building an entire workspace *)
type workspace_result = {
  results : Package_builder.build_result list;
  total_duration : Time.Duration.t;
  cached_count : int;
  built_count : int;
  failed_count : int;
  package_graph : Package_graph.t;
}

(** Build an entire workspace with parallel package builds.
    
    This coordinator manages building multiple packages concurrently,
    respecting dependency order. Workers build packages in parallel
    when dependencies allow.
    
    @param workspace The workspace to build
    @param toolchain The OCaml toolchain to use
    @param store The artifact store for caching
    @param target Which packages to build (all or specific package)
    @param concurrency Number of parallel workers
    @return Ok with workspace_result on success, Error with planning error on failure
*)
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
