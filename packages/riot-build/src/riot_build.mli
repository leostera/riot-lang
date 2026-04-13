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
type build_phase = Event.phase =
  | RuntimePhase of Event.runtime_phase
  | CliPhase of Event.cli_phase
type build_event = Event.t =
  | Pm of Riot_model.Event.t
  | BuildingTarget of { target: string; host: bool }
  | CacheGc of Riot_store.Cache_gc.event
  | Phase of build_phase
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
  profile: string;
  args: string list;
}
type source_run_request = Run_runtime.source_run_request = {
  source_spec: string;
  binary_name: string;
  profile: string;
  update: bool;
  args: string list;
}
type runnable_binary = Run_runtime.runnable_binary = {
  package_name: string;
  binary_name: string;
  source_path: Path.t;
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
  | ExternalTargetLoadFailed of { target: string; reason: string }
  | ClientError of Client.error
val build_scope_for_binary: Riot_model.Workspace.t -> package_name:string -> binary_name:string -> build_scope

val list_binaries: Riot_model.Workspace.t -> ?package_filter:string -> unit -> runnable_binary list

val run_error_message: run_error -> string

val run_event_to_json: run_event -> Data.Json.t option

val start_local:
  ?emit:(Riot_model.Event.kind -> unit) ->
  ?workspace_manager:Riot_model.Workspace_manager.t ->
  ?registry:Pkgs_ml.Registry.t ->
  ?registry_name:string ->
  workspace:Riot_model.Workspace.t ->
  config:Server_config.t ->
  unit ->
  (Pid.t, error) result

val build:
  ?on_event:(build_event -> unit) ->
  ?workspace_manager:Riot_model.Workspace_manager.t ->
  build_request ->
  (Riot_executor.Package_builder.build_result list, build_error) result

val build_prepared:
  ?on_event:(build_event -> unit) ->
  ?workspace_manager:Riot_model.Workspace_manager.t ->
  build_request ->
  (Riot_executor.Package_builder.build_result list, build_error) result

val run: ?on_event:(run_event -> unit) -> run_request -> (unit, run_error) result

val run_source: ?on_event:(run_event -> unit) -> source_run_request -> (unit, run_error) result

type suite_binary = Test_runtime.suite_binary = {
  package_name: string;
  suite_name: string;
}
type test_request = Test_runtime.test_request = {
  workspace: Riot_model.Workspace.t;
  package_filter: string option;
  suite_filter: string option;
  profile: string;
  extra_args: string list;
}
type test_case_type = Test_runtime.test_case_type =
  | Test
  | Property of { examples: int }
type test_case_size = Test_runtime.test_case_size =
  | Small
  | Large
type test_case_reliability = Test_runtime.test_case_reliability =
  | Stable
  | Flaky of { retry_attempts: int }
type test_case_status = Test_runtime.test_case_status =
  | Passed
  | Failed of string
  | Timed_out of { timeout_ms: int }
  | Skipped
type test_case_result = Test_runtime.test_case_result = {
  index: int;
  name: string;
  test_type: test_case_type;
  size: test_case_size;
  reliability: test_case_reliability;
  attempts: int;
  result: test_case_status;
  duration_us: int;
}
type listed_test_case = Test_runtime.listed_test_case = {
  index: int;
  name: string;
  test_type: test_case_type;
  size: test_case_size;
  reliability: test_case_reliability;
  skip: bool;
}
type listed_test_suite = Test_runtime.listed_test_suite = {
  suite: suite_binary;
  source_path: Path.t option;
  tests: listed_test_case list;
}
type failed_test = Test_runtime.failed_test = {
  suite: suite_binary;
  name: string;
  message: string;
  duration_us: int;
}
type test_suite_summary = Test_runtime.test_suite_summary = {
  total: int;
  passed: int;
  failed: int;
  skipped: int;
  duration_us: int;
  results: test_case_result list;
}
type test_event =
  | Build of build_event
  | NoSuitesFound of { package_name: string option; suite_name: string option }
  | RunningSuite of suite_binary
  | SuiteCompleted of {
      suite: suite_binary;
      status: int;
      stdout: string;
      stderr: string;
      started_at_us: int option;
      completed_at_us: int option;
      duration_us: int option;
      summary: test_suite_summary
    }
  | Summary of { total: int; passed: int; failed: int; skipped: int; failed_tests: failed_test list }
type test_error =
  | BuildFailed of build_error
  | ClientError of Client.error
  | SuiteArtifactNotFound of { suite: suite_binary; reason: string }
  | SuiteExecutionError of { suite: suite_binary; reason: string }
  | SuitesFailed of int
val collect_test_suites:
  Riot_model.Workspace.t -> ?package_filter:string -> ?suite_filter:string -> unit -> suite_binary list

val test_error_message: test_error -> string

val test_event_to_json: test_event -> Data.Json.t option

val list_tests:
  ?on_suite:(listed_test_suite -> unit) ->
  ?on_suite_error:(suite_binary -> test_error -> unit) ->
  test_request ->
  (listed_test_suite list, test_error) result

val test: ?on_event:(test_event -> unit) -> test_request -> (unit, test_error) result

type bench_request = Bench_runtime.bench_request = {
  workspace: Riot_model.Workspace.t;
  package_filter: string option;
  suite_filter: string option;
  profile: string;
  extra_args: string list;
}
type bench_statistics = Bench_runtime.bench_statistics = {
  min: Time.Duration.t;
  max: Time.Duration.t;
  mean: Time.Duration.t;
  median: Time.Duration.t;
  std_dev: Time.Duration.t;
  iterations: int;
  total_time: Time.Duration.t;
}
type bench_case_status = Bench_runtime.bench_case_status =
  | Completed of bench_statistics
  | Failed of string
  | Skipped
type bench_case_result = Bench_runtime.bench_case_result = {
  index: int;
  name: string;
  result: bench_case_status;
}
type listed_bench_item_kind = Bench_runtime.listed_bench_item_kind =
  | Benchmark
  | Comparison
type listed_bench_item = Bench_runtime.listed_bench_item = {
  index: int;
  name: string;
  kind: listed_bench_item_kind;
  iterations: int;
  warmup: int;
  skip: bool;
  cases: string list;
}
type listed_bench_suite = Bench_runtime.listed_bench_suite = {
  suite: suite_binary;
  source_path: Path.t option;
  benchmarks: listed_bench_item list;
}
type bench_comparison_case_result = Bench_runtime.bench_comparison_case_result = {
  name: string;
  statistics: bench_statistics;
}
type bench_comparison_result = Bench_runtime.bench_comparison_result = {
  description: string;
  case_results: bench_comparison_case_result list;
  fastest: string;
  speedup_ratios: (string * float) list;
}
type bench_suite_summary = Bench_runtime.bench_suite_summary = {
  total: int;
  completed: int;
  skipped: int;
  failed: int;
}
type bench_event =
  | Build of build_event
  | NoSuitesFound of { package_name: string option }
  | RunningSuite of suite_binary
  | SuiteCompleted of {
      suite: suite_binary;
      status: int;
      stdout: string;
      stderr: string;
      started_at_us: int option;
      completed_at_us: int option;
      duration_us: int option;
      results: bench_case_result list;
      comparisons: bench_comparison_result list;
      summary: bench_suite_summary
    }
  | Summary of { total: int; completed: int; skipped: int; failed: int }
type bench_error =
  | BuildFailed of build_error
  | ClientError of Client.error
  | SuiteArtifactNotFound of { suite: suite_binary; reason: string }
  | SuiteExecutionError of { suite: suite_binary; reason: string }
  | SuitesFailed of int
val collect_bench_suites:
  Riot_model.Workspace.t -> ?package_filter:string -> ?suite_filter:string -> unit -> suite_binary list

val bench_error_message: bench_error -> string

val bench_event_to_json: bench_event -> Data.Json.t option

val list_benchmarks:
  ?on_suite:(listed_bench_suite -> unit) ->
  ?on_suite_error:(suite_binary -> bench_error -> unit) ->
  bench_request ->
  (listed_bench_suite list, bench_error) result

val bench: ?on_event:(bench_event -> unit) -> bench_request -> (unit, bench_error) result

type install_request = Install_runtime.install_request = {
  workspace: Riot_model.Workspace.t;
  package_name: string option;
  binary_name: string;
  local_only: bool;
  promote_to_workspace_root: bool;
}
type source_install_request = Install_runtime.source_install_request = {
  source_spec: string;
  binary_name: string;
  update: bool;
  local_only: bool;
}
type registry_install_request = Install_runtime.registry_install_request = {
  package_spec: string;
  binary_name: string;
  local_only: bool;
}
type install_event =
  | Build of build_event
  | InstallingBinary of { package: string; binary: string }
  | PromotedBinary of { binary: string; destination: Path.t; global: bool }
  | InstalledBinary of { binary: string; duration_ms: int; global_destination: Path.t option }
type install_error =
  | BinaryNotFound of { binary_name: string }
  | BinaryNotFoundInPackage of { package_name: string; binary_name: string }
  | BuildFailed of build_error
  | ArtifactNotFound of { package_name: string; binary_name: string; reason: string }
  | PromotionFailed of { binary_name: string; destination: Path.t; global: bool; reason: string }
  | ExternalTargetLoadFailed of { target: string; reason: string }
  | ClientError of Client.error
val install_error_message: install_error -> string

val install_event_to_json: install_event -> Data.Json.t option

val install: ?on_event:(install_event -> unit) -> install_request -> (unit, install_error) result

val install_source:
  ?on_event:(install_event -> unit) -> source_install_request -> (unit, install_error) result

val install_registry:
  ?on_event:(install_event -> unit) -> registry_install_request -> (unit, install_error) result
