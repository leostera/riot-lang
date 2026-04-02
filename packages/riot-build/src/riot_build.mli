open Std

module Client = Client

module Event = Event

module Internal_server = Internal_server

module Protocol = Protocol

module Server_config = Server_config

type error = Internal_server.error
val error_message: error -> string

type build_scope =
  | Runtime
  | Dev
type target_request =
  | Host
  | All
  | Pattern of string
type build_request = {
  workspace: Riot_model.Workspace.t;
  packages: string list;
  targets: target_request;
  scope: build_scope;
  profile: string;
}
type build_event = Event.t =
  | Pm of Riot_model.Event.t
  | BuildingTarget of { target: string; host: bool }
  | Streaming of Client.streaming_event
type build_error =
  | NoTargetsMatched of { pattern: string; available_targets: string list }
  | ToolchainInstallFailed of { target: string; error: string }
  | ToolchainInitializationFailed of { target: string; error: string }
  | ClientError of Client.error
val build_error_message: build_error -> string

type run_request = Run_runtime.run_request = {
  workspace: Riot_model.Workspace.t;
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
val build_scope_for_binary: Riot_model.Workspace.t -> package_name:string -> binary_name:string -> build_scope

val run_error_message: run_error -> string

val run_event_to_json: run_event -> Data.Json.t option

val start_local:
  ?emit:(Riot_model.Event.kind -> unit) ->
  ?registry:Pkgs_ml.Registry.t ->
  ?registry_name:string ->
  workspace:Riot_model.Workspace.t ->
  config:Server_config.t ->
  unit ->
  (Pid.t, error) result

val build: ?on_event:(build_event -> unit) -> build_request -> (unit, build_error) result

val run: ?on_event:(run_event -> unit) -> run_request -> (unit, run_error) result

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
type test_event =
  | Build of build_event
  | NoSuitesFound of { package_name: string option }
  | RunningSuite of suite_binary
  | SuiteCompleted of { suite: suite_binary; status: int; stdout: string; stderr: string }
  | Summary of { total: int; passed: int; failed: int }
type test_error =
  | BuildFailed of build_error
  | ClientError of Client.error
  | SuiteArtifactNotFound of { suite: suite_binary; reason: string }
  | SuiteExecutionError of { suite: suite_binary; reason: string }
  | SuitesFailed of int
val collect_test_suites: Riot_model.Workspace.t -> ?package_filter:string -> unit -> suite_binary list

val test_error_message: test_error -> string

val test_event_to_json: test_event -> Data.Json.t option

val test: ?on_event:(test_event -> unit) -> test_request -> (unit, test_error) result

type bench_request = Bench_runtime.bench_request = {
  workspace: Riot_model.Workspace.t;
  package_filter: string option;
  query: string option;
  extra_args: string list;
}
type bench_event =
  | Build of build_event
  | NoSuitesFound of { package_name: string option }
  | RunningSuite of suite_binary
  | SuiteCompleted of { suite: suite_binary; status: int; stdout: string; stderr: string }
  | Summary of { total: int; passed: int; failed: int }
type bench_error =
  | BuildFailed of build_error
  | ClientError of Client.error
  | SuiteArtifactNotFound of { suite: suite_binary; reason: string }
  | SuiteExecutionError of { suite: suite_binary; reason: string }
  | SuitesFailed of int
val collect_bench_suites: Riot_model.Workspace.t -> ?package_filter:string -> unit -> suite_binary list

val bench_error_message: bench_error -> string

val bench_event_to_json: bench_event -> Data.Json.t option

val bench: ?on_event:(bench_event -> unit) -> bench_request -> (unit, bench_error) result

type install_request = Install_runtime.install_request = {
  workspace: Riot_model.Workspace.t;
  binary_name: string;
  local_only: bool;
}
type install_event =
  | Build of build_event
  | InstallingBinary of { package: string; binary: string }
  | PromotedBinary of { binary: string; destination: Path.t; global: bool }
  | PromotionWarning of { binary: string; destination: Path.t; global: bool; reason: string }
  | InstalledBinary of { binary: string; duration_ms: int; global_destination: Path.t option }
type install_error =
  | BinaryNotFound of { binary_name: string }
  | BuildFailed of build_error
  | ArtifactNotFound of { package_name: string; binary_name: string; reason: string }
  | ClientError of Client.error
val install_error_message: install_error -> string

val install_event_to_json: install_event -> Data.Json.t option

val install: ?on_event:(install_event -> unit) -> install_request -> (unit, install_error) result
