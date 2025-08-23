(** Build planner - plans a build node. *)

type plan_result =
  | Planned of Build_node.t
  | MissingDependencies of { node : Build_node.t; deps : Build_node.t list }

type error = string

val plan_node :
  graph:Build_graph.t ->
  node:Build_node.t ->
  unit ->
  (plan_result, error) result
