open Std

module Event = Event
module Prepared_workspace = Prepared_workspace
module Request = Request
module Output = Output

module Commands: sig
  module Test = Test_runtime
  module Bench = Bench_runtime
end

module Internal: sig
  module Build_lock = Build_lock
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
  | BuildFailed of { errors: Riot_executor.Package_builder.build_result list }
  | PlanningFailed of { reason: string }
  | CycleDetected of { cycle_nodes: string list }
  | BuildAlreadyRunning of { lock_path: Path.t }
  | SessionStartFailed of { reason: string }
  | UnexpectedError of { reason: string }

val error_message: error -> string

val build:
  ?on_event:(Event.t -> unit) ->
  Request.t ->
  (Output.t, error) result
