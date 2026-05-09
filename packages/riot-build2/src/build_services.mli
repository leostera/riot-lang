open Std

type t

val create: config:Build_config.t -> unit -> t

val config: t -> Build_config.t

val catalog: t -> Package_catalog.t

val compute_dependencies: t -> Work_node.t -> (Work_node.key list, Error.t) result

val execute_node: t -> Work_registry.t -> Work_node.t -> (Work_result.t, Error.t) result

val package_results: t -> Build_result.package_result list
