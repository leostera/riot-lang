open Std

type t

val create:
  workspace:Riot_model.Workspace.t ->
  catalog:Package_catalog.t ->
  store:Riot_store.Store.t ->
  package_planning:Package_planning.t ->
  package_sandbox:Package_sandbox.t ->
  module_providers:Module_provider_registry.t ->
  source_analyzer:Source_analyzer.t ->
  unit ->
  t

val begin_execution: t -> unit

val find: t -> Goal.build_package -> Module_plan.t option

val source_analysis_sources: t -> Goal.build_package -> (Source_analysis.t list, Error.t) result

val ocaml_sources: t -> Goal.build_package -> (Rule.ocaml_source list, Error.t) result

val c_objects: t -> Goal.build_package -> (Rule.c_object list, Error.t) result

val plan_dependencies:
  t ->
  Work_registry.t ->
  Goal.build_package ->
  (Work_request.t list, Error.t) result

val execute: t -> Work_registry.t -> Goal.build_package -> (Work_result.t, Error.t) result
