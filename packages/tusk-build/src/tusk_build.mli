open Std

module Client = Client

module Event = Event

module Internal_server = Internal_server

module Protocol = Protocol

module Server_config = Server_config

type error = Internal_server.error

val error_message : error -> string

type build_scope =
  | Runtime
  | Dev

type target_request =
  | Host
  | All
  | Pattern of string

type build_request = {
  workspace: Tusk_model.Workspace.t;
  load_errors: Tusk_model.Workspace_manager.load_error list;
  packages: string list;
  targets: target_request;
  scope: build_scope;
  profile: string;
}

type build_event =
  Event.t =
  | Pm of Tusk_model.Event.t
  | BuildingTarget of { target: string; host: bool }
  | Streaming of Client.streaming_event

type build_error =
  | NoTargetsMatched of { pattern: string; available_targets: string list }
  | ToolchainInstallFailed of { target: string; error: string }
  | ToolchainInitializationFailed of { target: string; error: string }
  | ClientError of Client.error

val build_error_message : build_error -> string

type run_request = Run_runtime.run_request = {
  workspace: Tusk_model.Workspace.t;
  load_errors: Tusk_model.Workspace_manager.load_error list;
  current_dir: Path.t;
  package_name: string option;
  binary_name: string;
  args: string list;
}

type run_event =
  | Build of build_event
  | RunningBinary of { package: string; binary: string; args: string list }

type run_error =
  | BinaryNotFound of { binary_name: string }
  | BinaryNotFoundInPackage of { package_name: string; binary_name: string }
  | BuildFailed of build_error
  | ArtifactNotFound of { package_name: string; binary_name: string; reason: string }
  | ProcessExited of int
  | SystemError of string
  | ClientError of Client.error

val build_scope_for_binary:
  Tusk_model.Workspace.t ->
  package_name:string ->
  binary_name:string ->
  build_scope

val run_error_message : run_error -> string

val run_event_to_json : run_event -> Data.Json.t option

val start_local:
  ?emit:(Tusk_model.Event.kind -> unit) ->
  ?registry:Pkgs_ml.Registry.t ->
  ?registry_name:string ->
  workspace:Tusk_model.Workspace.t ->
  ?load_errors:Tusk_model.Workspace_manager.load_error list ->
  config:Server_config.t ->
  unit ->
  (Pid.t, error) result

val build:
  ?on_event:(build_event -> unit) ->
  build_request ->
  (unit, build_error) result

val run:
  ?on_event:(run_event -> unit) ->
  run_request ->
  (unit, run_error) result
