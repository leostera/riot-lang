open Std

val execute_node: Executor.context -> Work_node.t -> (Executor.execution, Error.t) result

val run_intent: config:Build_config.t -> User_intent.t -> Executor.summary

val completed_goals: Executor.summary -> Goal.t list
