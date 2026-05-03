(**
   Package-level build graph planner.

   This module provides a high-level interface for planning package builds:
   1. Creates module dependency graph from source files
   2. Wires dependencies using syntactic dependency analysis
   3. Generates action graph for parallel execution
*)
open Std
open Riot_model

(** Inputs required to plan one package build. *)
type plan_input = {
  package: Package.t;
  profile: Profile.t;
  ctx: Build_ctx.t;
  toolchain: Riot_toolchain.t;
  workspace: Workspace.t;
  source_groups: Module_graph.source_group list;
  depset: Dependency.t list;
  dependency_packages: Package.t list;
  store: Riot_store.Store.t;
}
(**
   Plan a complete build for a package.

   This function orchestrates the entire planning process:
   1. Scans source directory to build module graph
   2. Wires module dependencies via syntactic dependency analysis
   3. Topologically sorts modules
   4. Generates compilation actions
   5. Returns action graph ready for parallel execution

   Returns:
   - Planned { module_graph, action_graph, outputs } on success
   - Cycle { cycle } if circular dependencies detected
   - Error msg if planning fails
*)
type plan_result = {
  sources: Path.t list;
  module_graph: Module_node.t Graph.SimpleGraph.t;
  analyzed_modules: (Graph.SimpleGraph.Node_id.t * Module_graph.analyzed_module) list;
  action_graph: Action_graph.t;
}

val plan_node: plan_input -> (plan_result, Planning_error.t) result
