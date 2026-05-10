open Std

module Node = Node

module Node_id = Node_id

module Node_queue = Node_queue

module Runner = Runner

module Summary = ExecutionSummary

type node = Work_node.t
type summary = ExecutionSummary.t
type node_result = ExecutionSummary.node_result

val has_failures: summary -> bool

val run: services:Build_services.t -> seeds:Work_node.t list -> unit -> summary
