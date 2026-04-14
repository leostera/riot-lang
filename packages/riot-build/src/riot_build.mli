open Std

module Event = Event
module Prepared_workspace = Prepared_workspace
module Request = Request
module Output = Output
module Run = Run_runtime
module Install = Install_runtime
module Test = Test_runtime
module Bench = Bench_runtime
module Workspace_edit = Workspace_edit
module Build_lock = Build_lock

type error = Build_core.error =
  | TargetSelectionFailed of Riot_model.Target.resolve_error
  | PackageNotFound of { package_name: string; available_packages: string list }
  | PackagesNotFound of { package_names: string list; available_packages: string list }
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
  Prepared_workspace.t ->
  Request.t ->
  (Output.t, error) result
