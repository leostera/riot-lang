(** Build planner - plans a build node. *)

type plan_result =
  | Planned of Build_node.t
  | MissingDependencies of { node : Build_node.t; deps : Build_node.t list }

type error = string

val plan_node :
  graph:Build_graph.t ->
  node:Build_node.t ->
  build_results:Build_results.t ->
  unit ->
  (plan_result, error) result
(** Plan a build node by checking dependencies and computing build actions.
    Returns [Planned node] if the node can be built now, or
    [MissingDependencies] if dependencies need to be built first. Uses
    build_results to check which dependencies are already built. *)
