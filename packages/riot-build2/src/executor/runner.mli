open Std

type context = {
  registry: Work_registry.t;
}

type execution =
  | Complete of Work_node.key list
  | RequeueWithDependencies of Work_node.key list

type task_result = {
  node: Work_node.t;
  outcome: (execution, Error.t) result;
}

val run:
  config:Build_config.t ->
  seeds:Work_node.t list ->
  execute:(context -> Work_node.t -> (execution, Error.t) result) ->
  unit ->
  Summary.t
