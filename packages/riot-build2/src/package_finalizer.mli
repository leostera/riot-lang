open Std

type t

val create:
  workspace:Riot_model.Workspace.t ->
  store:Riot_store.Store.t ->
  package_planning:Package_planning.t ->
  module_planning:Module_planning.t ->
  action_executor:Action_executor.t ->
  unit ->
  t

val results: t -> Build_result.package_result list

val find: t -> Package_work.build_library -> Build_result.package_result option

val execute:
  t ->
  Work_registry.t ->
  Package_work.build_library ->
  (Executor.execution, Error.t) result
