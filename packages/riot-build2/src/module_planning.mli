open Std

type t

val create:
  workspace:Riot_model.Workspace.t ->
  catalog:Package_catalog.t ->
  store:Riot_store.Store.t ->
  package_planning:Package_planning.t ->
  source_analyzer:Source_analyzer.t ->
  unit ->
  t

val find: t -> Goal.build_package -> Module_plan.t option

val plan_dependencies:
  t ->
  Work_registry.t ->
  Goal.build_package ->
  (Work_node.key list, Error.t) result

val execute: t -> Work_registry.t -> Goal.build_package -> (Work_result.t, Error.t) result
