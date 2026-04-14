open Std

type resolve_error =
  | TargetSelectionFailed of Riot_model.Target.resolve_error
  | PackageNotFound of { package_name: string; available_packages: string list }
  | PackagesNotFound of { package_names: string list; available_packages: string list }

type build_error = Build_runtime.build_error =
  | NoTargetsMatched of Riot_model.Target.resolve_error
  | ToolchainInstallFailed of { target: Riot_model.Target.t; error: string }
  | ToolchainInitializationFailed of { target: Riot_model.Target.t; error: string }
  | ClientError of Client.error

val resolve_error_message: resolve_error -> string

val build_error_message: build_error -> string

val resolve:
  Prepared_workspace.t ->
  Request.t ->
  (Build_spec.t, resolve_error) result

val build:
  ?on_event:(Build_runtime.build_event -> unit) ->
  Build_spec.t ->
  (Output.t, build_error) result
