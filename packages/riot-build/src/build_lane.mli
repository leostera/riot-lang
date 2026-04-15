open Std

type error = string

type unresolved

type locked

type 'stage t

type outcome = {
  target: Riot_model.Target.t;
  results: Package_builder.build_result list;
  had_partial_failure: bool;
}

val target: 'a t -> Riot_model.Target.t

val workspace: 'a t -> Riot_model.Workspace.t

val package_names: 'a t -> Riot_model.Package_name.t list

val scope: 'a t -> Build_spec.scope

val profile_name: 'a t -> string

val session_id: 'a t -> Riot_model.Session_id.t

val host: 'a t -> Riot_model.Target.t

val build_ctx: 'a t -> Riot_model.Build_ctx.t

val toolchain: 'a t -> Riot_toolchain.t

val store: 'a t -> Riot_store.Store.t

val planner_target: 'a t -> Riot_planner.Workspace_planner.target

val package_plan: 'a t -> Riot_planner.Workspace_planner.package_plan

val package_graph: 'a t -> Riot_planner.Package_graph.t

val prepare:
  workspace:Riot_model.Workspace.t ->
  package_names:Riot_model.Package_name.t list ->
  scope:Build_spec.scope ->
  profile:Riot_model.Profile.t ->
  session_id:Riot_model.Session_id.t ->
  host:Riot_model.Target.t ->
  target:Riot_model.Target.t ->
  toolchain:Riot_toolchain.t ->
  toolchain_config:Riot_model.Toolchain_config.t ->
  parallelism:int ->
  (locked t, error) result

val execute:
  locked t ->
  (outcome, error) result
