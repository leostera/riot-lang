(** Riot Build - typed build interface *)
open Std

module Event = Event
module Request = Request
module Build_result = Build_result
module BuildLock = Build_lock

type toolchain_install_error = Build_core.toolchain_install_error =
  | ToolchainDownloadFailed of { message: string }

let toolchain_install_error_message = Build_core.toolchain_install_error_message

type toolchain_initialization_error = Build_core.toolchain_initialization_error =
  | ToolchainInitFailed of { message: string }

let toolchain_initialization_error_message = Build_core.toolchain_initialization_error_message

module Internal = struct
  module Action_scheduler = Action_scheduler
  module Action_executor = Action_executor
  module Build_lane = Build_lane
  module Build_context = Build_context
  module Build_core = Build_core
  module Build_unit_plan = Build_unit_plan
  module Graph_scheduler = Graph_scheduler
  module Package_scheduler = Package_scheduler
  module Build_work = Build_work
  module Lane_result = Lane_result
  module Package_builder = Package_builder
  module Build_runtime = Build_runtime
  module Resolved_build = Resolved_build
  module Diagnostic_rewrite = Diagnostic_rewrite
  module Sandbox = Sandbox
  module Telemetry_events = Telemetry_events
end

type error = Build_core.error =
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

let error_message = Build_core.error_message

let build = Build_core.build
