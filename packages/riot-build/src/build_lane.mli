open Std

type error =
  | BuildUnitPlanningFailed of Build_unit_plan.error
  | Failure of string
type unresolved
type locked
type build_plan
type 'stage t

val error_message: error -> string

val target: 'a t -> Riot_model.Target.t

val workspace: 'a t -> Riot_model.Workspace.t

val package_names: 'a t -> Riot_model.Package_name.t list

val scope: 'a t -> Resolved_build.scope

val profile_name: 'a t -> string

val session_id: 'a t -> Riot_model.Session_id.t

val host: 'a t -> Riot_model.Target.t

val build_ctx: 'a t -> Riot_model.Build_ctx.t

val toolchain: 'a t -> Riot_toolchain.t

val store: 'a t -> Riot_store.Store.t

val on_event: 'a t -> Event.t -> unit

val build_unit_plan: 'a t -> Build_unit_plan.t

val build_unit_graph: 'a t -> Riot_planner.Build_unit_graph.t

val build_units: 'a t -> Riot_planner.Build_unit.t list

val build_unit_keys: 'a t -> Riot_planner.Build_unit.key list

val build_unit: 'a t -> Riot_planner.Build_unit.key -> Riot_planner.Build_unit.t option

val build_unit_dependency_keys:
  'a t ->
  Riot_planner.Build_unit.key ->
  Riot_planner.Build_unit.key list

val plan_build_units: Build_context.t -> Resolved_build.t -> (build_plan, error) result

val prepare:
  Build_context.t ->
  build_plan ->
  target:Riot_model.Target.t ->
  toolchain:Riot_toolchain.t ->
  (locked t, error) result

val release: locked t -> unit
