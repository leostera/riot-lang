open Std

type error =
  | PlanningFailed of Riot_planner.Workspace_planner.plan_error
  | Failure of string
type unresolved
type locked
type workspace_plan
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

val planner_target: 'a t -> Riot_planner.Workspace_planner.target

val package_plan: 'a t -> Riot_planner.Workspace_planner.package_plan

val package_graph: 'a t -> Riot_planner.Package_graph.t

val package_keys: 'a t -> Riot_model.Package.key list

val plan_workspace:
  Build_context.t ->
  Resolved_build.t ->
  (workspace_plan, error) result

val clone_workspace_plan: workspace_plan -> workspace_plan

val prepare:
  Build_context.t ->
  workspace_plan ->
  target:Riot_model.Target.t ->
  toolchain:Riot_toolchain.t ->
  (locked t, error) result

val release: locked t -> unit
