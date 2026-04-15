(** Riot Build - typed build interface *)
open Std
module Event = Event
module Request = Request
module Build_result = Build_result
module BuildLock = Build_lock
module Action_executor = Action_executor
module Action_queue = Action_queue
module Package_builder = Package_builder
module Build_runtime = Build_runtime
module Coordinator = Coordinator
module Diagnostic_rewrite = Diagnostic_rewrite
module Sandbox = Sandbox
module Telemetry_events = Telemetry_events

module Internal = struct
  module Action_executor = Action_executor
  module Action_queue = Action_queue
  module Build_core = Build_core
  module Build_spec = Build_spec
  module Package_builder = Package_builder
  module Build_runtime = Build_runtime
  module Coordinator = Coordinator
  module Diagnostic_rewrite = Diagnostic_rewrite
  module Sandbox = Sandbox
  module Telemetry_events = Telemetry_events
end

type error = Build_core.error =
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
  | PlanningFailed of { reason: string }
  | CycleDetected of { cycle_nodes: string list }
  | BuildAlreadyRunning of { lock_path: Path.t }
  | SessionStartFailed of { reason: string }
  | InvalidRequestedParallelism of int
  | UnexpectedError of { reason: string }

let error_message = Build_core.error_message

let build = Build_core.build
