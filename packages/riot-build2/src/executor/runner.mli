open Std

type task_result

val run: services:Build_services.t -> seeds:Work_node.t list -> unit -> ExecutionSummary.t

val run_with_handlers:
  ?plan_dependencies:(Work_registry.t -> Work_node.t -> (Work_request.t list, Error.t) result) ->
  ?execution_mode:(Work_node.t -> Work_node.execution_mode) ->
  config:Build_config.t ->
  seeds:Work_node.t list ->
  execute:(Work_registry.t -> Work_node.t -> (Work_result.t, Error.t) result) ->
  unit ->
  ExecutionSummary.t
