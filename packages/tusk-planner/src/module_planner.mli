(** Build Planner - Orchestrates build graph creation, wiring, and action
    generation

    This module provides a high-level interface for planning package builds: 1.
    Creates module dependency graph from source files 2. Wires dependencies
    using ocamldep 3. Generates action graph for parallel execution *)

open Std
open Tusk_model

type plan_input = {
  package : Package.t;
  toolchain : Tusk_toolchain.t;
  workspace : Workspace.t;
  planning_root : Path.t;
  depset : Dependency.t list;
}

type plan_result = {
  sources : Path.t list;
  module_graph : Module_node.t Graph.SimpleGraph.t;
  action_graph : Action_graph.t;
}

val plan_node : plan_input -> (plan_result, Planning_error.t) result
(** Plan a complete build for a package.
    
    This function orchestrates the entire planning process:
    1. Scans source directory to build module graph
    2. Wires module dependencies via ocamldep  
    3. Topologically sorts modules
    4. Generates compilation actions
    5. Returns action graph ready for parallel execution
    
    Returns:
    - Planned { module_graph, action_graph, outputs } on success
    - Cycle { cycle } if circular dependencies detected
    - Error msg if planning fails
*)
