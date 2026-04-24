open Std

type error =
  | TargetSelectionFailed of Riot_model.Target.resolve_error
  | PackageNotFound of {
      package_name: Riot_model.Package_name.t;
      available_packages: Riot_model.Package_name.t list
    }
  | PackagesNotFound of {
      package_names: Riot_model.Package_name.t list;
      available_packages: Riot_model.Package_name.t list
    }
  | ToolchainInstallFailed of { target: Riot_model.Target.t; error: string }
  | ToolchainInitializationFailed of { target: Riot_model.Target.t; error: string }
  | BuildFailed of { errors: Build_result.failure list }
  | PlanningFailed of Riot_planner.Workspace_planner.plan_error
  | CycleDetected of { cycle_nodes: string list }
  | BuildAlreadyRunning of { lock_path: Path.t }
  | InvalidRequestedParallelism of int
  | UnexpectedError of { reason: string }
val error_message: error -> string

val make_context: ?on_event:(Event.t -> unit) -> Request.t -> (Build_context.t, error) result

val resolve: Build_context.t -> Request.t -> (Resolved_build.t, error) result

val execute_raw:
  ?allow_partial_failures:bool ->
  ?record_cache_generation:bool ->
  Build_context.t ->
  Resolved_build.t ->
  (Package_builder.build_result list, error) result

val execute: Build_context.t -> Resolved_build.t -> (Build_result.t, error) result

val build: ?on_event:(Event.t -> unit) -> Request.t -> (Build_result.t, error) result
