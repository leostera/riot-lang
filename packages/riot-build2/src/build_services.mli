open Std

type t

val create: config:Build_config.t -> unit -> t

val begin_execution: t -> unit

val config: t -> Build_config.t

val catalog: t -> Package_catalog.t

val action_results: t -> Action_execution.result list

val module_plan: t -> Goal.build_package -> Module_plan.t option

val plan_dependencies: t -> Work_registry.t -> Work_node.t -> (Work_request.t list, Error.t) result

val execute_node: t -> Work_registry.t -> Work_node.t -> (Work_result.t, Error.t) result

val package_results: t -> Build_result.package_result list
