open Std
open Std.Collections
open Riot_model
open Riot_planner

(** Package build errors *)
type package_error =
  | PlanningFailed of Riot_planner.Planning_error.t
  | ExecutionFailed of { message: string }
  | ActionExecutionFailed of { message: string }
  | ActionOutputsNotCreated of {
      missing: Path.t list;
    }
  | ActionDependenciesFailed of {
      failed: Graph.SimpleGraph.Node_id.t list;
    }

val package_error_to_string: package_error -> string

val package_error_to_json: package_error -> Std.Data.Json.t

(** Build status for a package *)
type build_status =
  | Cached of Riot_store.Artifact.t
  | Built of Riot_store.Artifact.t
  | Skipped of { reason: string }
  | Failed of package_error

val build_status_to_json: build_status -> Std.Data.Json.t

(** Result of building a package *)
type build_result = {
  package_key: Package.key;
  package: Package.t;
  status: build_status;
  ocamlc_warnings: string list;
  duration: Time.Duration.t;
}

val build_result_to_json: build_result -> Std.Data.Json.t

type graph_update =
  | Planned_package of {
      hash: Std.Crypto.hash;
      module_graph: Module_node.t Graph.SimpleGraph.t;
      action_graph: Action_graph.t;
    }
  | Cached_package of {
      hash: Std.Crypto.hash;
      artifact: Riot_store.Artifact.t;
      depset: Dependency.t list;
      exports: Riot_store.Store.export_entry list;
    }
  | Built_package of {
      hash: Std.Crypto.hash;
      artifact: Riot_store.Artifact.t;
      depset: Dependency.t list;
      module_graph: Module_node.t Graph.SimpleGraph.t;
      action_graph: Action_graph.t;
      status: Package_graph.build_status;
    }
  | Failed_package of {
      hash: Std.Crypto.hash option;
      error: string;
    }
  | Skipped_package of { reason: string }
type detailed_result = {
  result: build_result;
  graph_update: graph_update option;
}
type execution_plan = {
  package_key: Package.key;
  package: Package.t;
  module_graph: Module_node.t Graph.SimpleGraph.t;
  action_graph: Action_graph.t;
  hash: Std.Crypto.hash;
  depset: Dependency.t list;
  started_at: Time.Instant.t;
  emit_visible_progress: bool;
}
type plan_outcome =
  | Final_result of detailed_result
  | Execution_required of execution_plan
type prepared_execution = {
  execution_plan: execution_plan;
  sandbox: Sandbox.t;
  toolchain: Riot_toolchain.t;
}

val apply_graph_update: Package_graph.t -> Package.key -> Package.t -> graph_update option -> unit

val planned_graph_update: execution_plan -> graph_update

(**
   Collect all source files (.ml, .mli, .c, .h) from a package's src directory.

   @param package The package to scan
   @return List of absolute paths to source files
*)
val collect_source_files: Package.t -> Path.t list

(**
   Build a single package.

   Returns a build_result indicating whether the package was cached, built, or failed.

   @param workspace The workspace containing the package
   @param toolchain The OCaml toolchain to use
   @param store The artifact store for caching
   @param package_graph The dependency graph for all packages
   @param package The package to build
*)
val build:
  workspace:Workspace.t ->
  toolchain:Riot_toolchain.t ->
  store:Riot_store.Store.t ->
  package_graph:Package_graph.t ->
  package_key:Package.key ->
  package:Package.t ->
  build_ctx:Build_ctx.t ->
  build_result

val plan_detailed:
  workspace:Workspace.t ->
  toolchain:Riot_toolchain.t ->
  store:Riot_store.Store.t ->
  package_graph:Package_graph.t ->
  package_key:Package.key ->
  package:Package.t ->
  build_ctx:Build_ctx.t ->
  plan_outcome

val prepare_execution:
  workspace:Workspace.t ->
  toolchain:Riot_toolchain.t ->
  store:Riot_store.Store.t ->
  execution_plan:execution_plan ->
  build_ctx:Build_ctx.t ->
  (prepared_execution, detailed_result) result

val execute_action:
  store:Riot_store.Store.t ->
  prepared_execution:prepared_execution ->
  build_ctx:Build_ctx.t ->
  completed:(Graph.SimpleGraph.Node_id.t, Action_executor.execution_result) HashMap.t ->
  Action_node.t ->
  Action_executor.execution_result

val finalize_execution:
  workspace:Workspace.t ->
  store:Riot_store.Store.t ->
  prepared_execution:prepared_execution ->
  completed:(Graph.SimpleGraph.Node_id.t, Action_executor.execution_result) HashMap.t ->
  build_ctx:Build_ctx.t ->
  detailed_result
