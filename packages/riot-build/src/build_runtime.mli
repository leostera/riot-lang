open Std

type build_error =
  | ToolchainInstallFailed of { target: Riot_model.Target.t; error: string }
  | ToolchainInitializationFailed of { target: Riot_model.Target.t; error: string }
  | BuildFailed of { errors: Package_builder.build_result list }
  | PlanningFailed of Riot_planner.Workspace_planner.plan_error
  | UnexpectedError of { reason: string }
val error_message: build_error -> string

val execute:
  ?allow_partial_failures:bool ->
  ?record_cache_generation:bool ->
  Build_context.t ->
  Resolved_build.t ->
  (Package_builder.build_result list, build_error) result
