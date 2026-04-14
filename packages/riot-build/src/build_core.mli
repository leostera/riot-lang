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
  | BuildFailed of { errors: Riot_executor.Package_builder.build_result list }
  | PlanningFailed of { reason: string }
  | CycleDetected of { cycle_nodes: string list }
  | BuildAlreadyRunning of { lock_path: Path.t }
  | SessionStartFailed of { reason: string }
  | UnexpectedError of { reason: string }

val error_message: error -> string

val resolve:
  Request.t ->
  (Build_spec.t, error) result

val execute_raw:
  ?allow_partial_failures:bool ->
  ?record_cache_generation:bool ->
  ?on_event:(Event.t -> unit) ->
  Build_spec.t ->
  (Riot_executor.Package_builder.build_result list, error) result

val execute:
  ?on_event:(Event.t -> unit) ->
  Build_spec.t ->
  (Build_result.t, error) result

val build:
  ?on_event:(Event.t -> unit) ->
  Request.t ->
  (Build_result.t, error) result
