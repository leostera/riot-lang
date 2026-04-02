(** Riot Build - Exports the local build session runtime *)
open Std
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
  workspace: Riot_model.Workspace.t;
  packages: string list;
  targets: target_request;
  scope: build_scope;
  profile: string;
}

type build_event = Build_runtime.build_event =
  | Pm of Riot_model.Event.t
  | BuildingTarget of { target: string; host: bool }
  | Streaming of Client.streaming_event

type build_error = Build_runtime.build_error =
  | NoTargetsMatched of { pattern: string; available_targets: string list }
  | ToolchainInstallFailed of { target: string; error: string }
  | ToolchainInitializationFailed of { target: string; error: string }
  | ClientError of Client.error

type run_request = Run_runtime.run_request = {
  workspace: Riot_model.Workspace.t;
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

type suite_binary = Test_runtime.suite_binary = {
  package_name: string;
  suite_name: string;
}

type test_request = Test_runtime.test_request = {
  workspace: Riot_model.Workspace.t;
  package_filter: string option;
  query: string option;
  extra_args: string list;
}

type test_event = Test_runtime.test_event =
  | Build of build_event
  | NoSuitesFound of { package_name: string option }
  | RunningSuite of suite_binary
  | SuiteCompleted of { suite: suite_binary; status: int; stdout: string; stderr: string }
  | Summary of { total: int; passed: int; failed: int }

type test_error = Test_runtime.test_error =
  | BuildFailed of build_error
  | ClientError of Client.error
  | SuiteArtifactNotFound of { suite: suite_binary; reason: string }
  | SuiteExecutionError of { suite: suite_binary; reason: string }
  | SuitesFailed of int

let collect_test_suites = Test_runtime.collect_suite_binaries

let test_error_message = Test_runtime.test_error_message

let test_event_to_json = Test_runtime.test_event_to_json

let test = Test_runtime.test

type bench_request = Bench_runtime.bench_request = {
  workspace: Riot_model.Workspace.t;
  package_filter: string option;
  query: string option;
  extra_args: string list;
}

type bench_event = Bench_runtime.bench_event =
  | Build of build_event
  | NoSuitesFound of { package_name: string option }
  | RunningSuite of suite_binary
  | SuiteCompleted of { suite: suite_binary; status: int; stdout: string; stderr: string }
  | Summary of { total: int; passed: int; failed: int }

type bench_error = Bench_runtime.bench_error =
  | BuildFailed of build_error
  | ClientError of Client.error
  | SuiteArtifactNotFound of { suite: suite_binary; reason: string }
  | SuiteExecutionError of { suite: suite_binary; reason: string }
  | SuitesFailed of int

let collect_bench_suites = Bench_runtime.collect_suite_binaries

let bench_error_message = Bench_runtime.bench_error_message

let bench_event_to_json = Bench_runtime.bench_event_to_json

let bench = Bench_runtime.bench

type install_request = Install_runtime.install_request = {
  workspace: Riot_model.Workspace.t;
  binary_name: string;
  local_only: bool;
}

type install_event = Install_runtime.install_event =
  | Build of build_event
  | InstallingBinary of { package: string; binary: string }
  | PromotedBinary of { binary: string; destination: Path.t; global: bool }
  | PromotionWarning of { binary: string; destination: Path.t; global: bool; reason: string }
  | InstalledBinary of { binary: string; duration_ms: int; global_destination: Path.t option }

type install_error = Install_runtime.install_error =
  | BinaryNotFound of { binary_name: string }
  | BuildFailed of build_error
  | ArtifactNotFound of { package_name: string; binary_name: string; reason: string }
  | ClientError of Client.error

let install_error_message = Install_runtime.install_error_message

let install_event_to_json = Install_runtime.install_event_to_json

let install = Install_runtime.install
