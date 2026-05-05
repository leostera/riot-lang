open Std

type toolchain_install_error = Build_runtime.toolchain_install_error =
  | ToolchainDownloadFailed of { message: string }

val toolchain_install_error_message: toolchain_install_error -> string

type toolchain_initialization_error = Build_runtime.toolchain_initialization_error =
  | ToolchainInitFailed of { message: string }

val toolchain_initialization_error_message: toolchain_initialization_error -> string

type error =
  | TargetSelectionFailed of Riot_model.Target.resolve_error
  | PackageNotFound of {
      package_name: Riot_model.Package_name.t;
      available_packages: Riot_model.Package_name.t list;
    }
  | PackagesNotFound of {
      package_names: Riot_model.Package_name.t list;
      available_packages: Riot_model.Package_name.t list;
    }
  | ToolchainInstallFailed of {
      target: Riot_model.Target.t;
      error: toolchain_install_error;
    }
  | ToolchainInitializationFailed of {
      target: Riot_model.Target.t;
      error: toolchain_initialization_error;
    }
  | BuildFailed of {
      errors: Build_result.failure list;
    }
  | BuildUnitPlanningFailed of Build_unit_plan.error
  | CycleDetected of {
      cycle_nodes: string list;
    }
  | BuildAlreadyRunning of {
      lock_path: Path.t;
    }
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
