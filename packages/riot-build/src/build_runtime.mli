open Std

type toolchain_install_error =
  | ToolchainDownloadFailed of { message: string }

val toolchain_install_error_message: toolchain_install_error -> string

type toolchain_initialization_error =
  | ToolchainInitFailed of { message: string }

val toolchain_initialization_error_message: toolchain_initialization_error -> string

type build_error =
  | ToolchainInstallFailed of {
      target: Riot_model.Target.t;
      error: toolchain_install_error;
    }
  | ToolchainInitializationFailed of {
      target: Riot_model.Target.t;
      error: toolchain_initialization_error;
    }
  | BuildFailed of {
      errors: Package_builder.build_result list;
    }
  | BuildUnitPlanningFailed of Build_unit_plan.error
  | UnexpectedError of { reason: string }

val error_message: build_error -> string

val execute:
  ?allow_partial_failures:bool ->
  ?record_cache_generation:bool ->
  Build_context.t ->
  Resolved_build.t ->
  (Package_builder.build_result list, build_error) result
