open Std

type build_event = Event.t
type build_error =
  | ToolchainInstallFailed of { target: Riot_model.Target.t; error: string }
  | ToolchainInitializationFailed of { target: Riot_model.Target.t; error: string }
  | InvalidRequestedParallelism of int
  | PackageNotFound of {
      package_name: Riot_model.Package_name.t;
      available_packages: Riot_model.Package_name.t list
    }
  | PackagesNotFound of {
      package_names: Riot_model.Package_name.t list;
      available_packages: Riot_model.Package_name.t list
    }
  | BuildFailed of { errors: Package_builder.build_result list }
  | PlanningFailed of { reason: string }
  | CycleDetected of { cycle_nodes: string list }
  | BuildAlreadyRunning of { lock_path: Path.t }
  | UnexpectedError of { reason: string }
val error_message: build_error -> string

val execute:
  ?allow_partial_failures:bool ->
  ?record_cache_generation:bool ->
  ?on_event:(build_event -> unit) ->
  Build_spec.t ->
  (Package_builder.build_result list, build_error) result
