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

val find: t -> Package_work.build_library -> Module_plan.t option

val execute:
  t ->
  Work_registry.t ->
  Package_work.build_library ->
  (Executor.execution, Error.t) result
