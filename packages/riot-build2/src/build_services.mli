open Std

type t

val create: config:Build_config.t -> unit -> t

val config: t -> Build_config.t

val catalog: t -> Package_catalog.t

val execute_node: t -> Executor.context -> Work_node.t -> (Executor.execution, Error.t) result

val package_results: t -> Build_result.package_result list
