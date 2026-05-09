open Std

type t

val create:
  workspace:Riot_model.Workspace.t ->
  catalog:Package_catalog.t ->
  store:Riot_store.Store.t ->
  session_id:Riot_model.Session_id.t ->
  parallelism:int ->
  toolchains:Toolchain_service.t ->
  source_analyzer:Source_analyzer.t ->
  unit ->
  t

val find: t -> Package_work.build_library -> Module_plan.t option

val execute:
  t ->
  Work_registry.t ->
  Package_work.build_library ->
  (Executor.execution, Error.t) result
