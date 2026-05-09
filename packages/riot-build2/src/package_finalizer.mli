open Std

type t

val create:
  workspace:Riot_model.Workspace.t ->
  catalog:Package_catalog.t ->
  store:Riot_store.Store.t ->
  package_planning:Package_planning.t ->
  module_planning:Module_planning.t ->
  action_executor:Action_executor.t ->
  unit ->
  t

val results: t -> Build_result.package_result list

val find: t -> Goal.build_package -> Build_result.package_result option

val plan_dependencies:
  t ->
  Work_registry.t ->
  Goal.build_package ->
  (Work_node.key list, Error.t) result

val plan_artifact_dependencies:
  t ->
  Work_registry.t ->
  Goal.build_package ->
  (Work_node.key list, Error.t) result

val execute: t -> Work_registry.t -> Goal.build_package -> (Work_result.t, Error.t) result

val execute_artifact: t -> Work_registry.t -> Goal.build_package -> (Work_result.t, Error.t) result
