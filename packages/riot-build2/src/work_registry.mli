open Std

type t

val create: ?next_id:int -> ?capacity:int -> unit -> t

val find: t -> Work_node.key -> Work_node.t option

val find_by_id: t -> Work_node.Node_id.t -> Work_node.t option

val register: t -> Work_node.t -> Work_node.t

val intern: t -> key:Work_node.key -> make:(unit -> Work_node.kind) -> Work_node.t

val intern_package: t -> Riot_model.Package_name.t -> make:(unit -> Work_node.kind) -> Work_node.t

val find_package: t -> Riot_model.Package_name.t -> Work_node.t option

val intern_module:
  t ->
  package:Riot_model.Package_name.t option ->
  scope:string option ->
  name:string ->
  make:(unit -> Work_node.kind) ->
  Work_node.t

val find_module:
  t ->
  package:Riot_model.Package_name.t option ->
  scope:string option ->
  name:string ->
  Work_node.t option

val intern_goal: t -> Goal.t -> Work_node.t

val find_goal: t -> Goal.t -> Work_node.t option

val intern_toolchain_ready: t -> Toolchain_ready.t -> Work_node.t

val intern_source_analysis: t -> Source_analysis.t -> Work_node.t

val intern_module_plan: t -> Goal.build_package -> Work_node.t

val intern_action_execution: t -> Action_execution.t -> Work_node.t
