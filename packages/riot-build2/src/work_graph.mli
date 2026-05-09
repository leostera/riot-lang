open Std

val execute_node: Work_registry.t -> Work_node.t -> (Work_result.t, Error.t) result

val run_intent: config:Build_config.t -> User_intent.t -> ExecutionSummary.t

val completed_goals: ExecutionSummary.t -> Goal.t list
