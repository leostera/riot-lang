open Std

module Node = Node

module Node_id = Node_id

module Node_queue = Node_queue

module Runner = Runner

module Summary = Summary

type node = Work_node.t
type context = Runner.context = {
  registry: Work_registry.t;
}
type execution = Runner.execution =
  | Complete of Work_node.key list
  | RequeueWithDependencies of Work_node.key list
type summary = Summary.t
type node_result = Summary.node_result

val has_failures: summary -> bool

val run:
  config:Build_config.t ->
  seeds:Work_node.t list ->
  execute:(context -> Work_node.t -> (execution, Error.t) result) ->
  unit ->
  summary
