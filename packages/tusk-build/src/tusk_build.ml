(** Tusk Build - Exports the local build session runtime *)
module Build_server = Build_server
module Client = Client
module Event = Event
module Internal_server = Internal_server
module Protocol = Protocol
module Server_config = Server_config

type error = Internal_server.error

type build_scope = Build_runtime.build_scope =
  | Runtime
  | Dev

type target_request = Build_runtime.target_request =
  | Host
  | All
  | Pattern of string

type build_request = Build_runtime.build_request = {
  workspace: Tusk_model.Workspace.t;
  load_errors: Tusk_model.Workspace_manager.load_error list;
  packages: string list;
  targets: target_request;
  scope: build_scope;
  profile: string;
}

type build_event = Build_runtime.build_event =
  | Pm of Tusk_model.Event.t
  | BuildingTarget of { target: string; host: bool }
  | Streaming of Client.streaming_event

type build_error = Build_runtime.build_error =
  | NoTargetsMatched of { pattern: string; available_targets: string list }
  | ToolchainInstallFailed of { target: string; error: string }
  | ToolchainInitializationFailed of { target: string; error: string }
  | ClientError of Client.error

type run_request = Run_runtime.run_request = {
  workspace: Tusk_model.Workspace.t;
  load_errors: Tusk_model.Workspace_manager.load_error list;
  current_dir: Std.Path.t;
  package_name: string option;
  binary_name: string;
  args: string list;
}

type run_event = Run_runtime.run_event =
  | Build of build_event
  | RunningBinary of { package: string; binary: string; args: string list }

type run_error = Run_runtime.run_error =
  | BinaryNotFound of { binary_name: string }
  | BinaryNotFoundInPackage of { package_name: string; binary_name: string }
  | BuildFailed of build_error
  | ArtifactNotFound of { package_name: string; binary_name: string; reason: string }
  | ProcessExited of int
  | SystemError of string
  | ClientError of Client.error

let error_message = Internal_server.error_message

let build_error_message = Build_runtime.error_message

let build_scope_for_binary = Run_runtime.build_scope_for_binary

let run_error_message = Run_runtime.run_error_message

let run_event_to_json = Run_runtime.run_event_to_json

let start_local = Internal_server.start_local

let build = Build_runtime.build

let run = Run_runtime.run
