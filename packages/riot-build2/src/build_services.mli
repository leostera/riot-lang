open Std

type t

val create: workspace:Riot_model.Workspace.t -> ?parallelism:int -> unit -> t

val catalog: t -> Package_catalog.t

val execute_node: t -> Executor.context -> Work_node.t -> (Executor.execution, Error.t) result

val package_results: t -> Build_result.package_result list
