open Std

type t

val create:
  workspace:Riot_model.Workspace.t ->
  catalog:Package_catalog.t ->
  store:Riot_store.Store.t ->
  package_planning:Package_planning.t ->
  package_sandbox:Package_sandbox.t ->
  module_planning:Module_planning.t ->
  action_executor:Action_executor.t ->
  unit ->
  t

val begin_execution: t -> unit

val package_results: t -> Build_result.package_result list

val find_package_result: t -> Goal.build_package -> Build_result.package_result option

val plan_goal_dependencies: t -> Goal.build_package -> (Work_request.t list, Error.t) result

val plan_package_artifact_dependencies:
  t ->
  Goal.build_package ->
  (Work_request.t list, Error.t) result

val execute_package_artifact:
  t ->
  Work_registry.t ->
  Goal.build_package ->
  (Work_result.t, Error.t) result

val plan_module_dependencies: t -> Goal.build_package -> (Work_request.t list, Error.t) result

val execute_module_dependencies:
  t ->
  Work_registry.t ->
  Goal.build_package ->
  (Work_result.t, Error.t) result

val plan_ocaml_archive: t -> Goal.build_package -> (Work_request.t list, Error.t) result

val execute_ocaml_archive:
  t ->
  Work_registry.t ->
  Goal.build_package ->
  (Work_result.t, Error.t) result

val plan_ocaml_source: t -> Rule.ocaml_source -> (Work_request.t list, Error.t) result

val plan_ocaml_generated: t -> Rule.ocaml_generated -> (Work_request.t list, Error.t) result

val execute_ocaml_interface:
  t ->
  Work_registry.t ->
  Rule.ocaml_source ->
  (Work_result.t, Error.t) result

val execute_ocaml_byte_implementation:
  t ->
  Work_registry.t ->
  Rule.ocaml_source ->
  (Work_result.t, Error.t) result

val execute_ocaml_implementation:
  t ->
  Work_registry.t ->
  Rule.ocaml_source ->
  (Work_result.t, Error.t) result

val execute_ocaml_generated:
  t ->
  Work_registry.t ->
  Rule.ocaml_generated ->
  (Work_result.t, Error.t) result

val plan_c_object: t -> Rule.c_object -> (Work_request.t list, Error.t) result

val execute_c_object: t -> Work_registry.t -> Rule.c_object -> (Work_result.t, Error.t) result
