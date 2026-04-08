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
  | CacheGc of Riot_store.Cache_gc.event
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
  | ExternalTargetLoadFailed of { target: string; reason: string }
  | ClientError of Client.error

let error_message = Internal_server.error_message

let build_error_message = Build_runtime.error_message

let build_scope_for_binary = Run_runtime.build_scope_for_binary

let list_binaries = Run_runtime.list_binaries

let run_error_message = Run_runtime.run_error_message

let run_event_to_json = Run_runtime.run_event_to_json

let start_local = Internal_server.start_local

let build = fun ?on_event ?workspace_manager request ->
  Build_runtime.build ?on_event ?workspace_manager request

let build_prepared = fun ?on_event ?workspace_manager request ->
  Build_runtime.build_prepared ?on_event ?workspace_manager request

let run = Run_runtime.run

let run_source = Run_runtime.run_source

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

type test_event = Test_runtime.test_event =
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

type test_error = Test_runtime.test_error =
  | BuildFailed of build_error
  | ClientError of Client.error
  | SuiteArtifactNotFound of { suite: suite_binary; reason: string }
  | SuiteExecutionError of { suite: suite_binary; reason: string }
  | SuitesFailed of int

let collect_test_suites = Test_runtime.collect_suite_binaries

let test_error_message = Test_runtime.test_error_message

let test_event_to_json = Test_runtime.test_event_to_json

let list_tests = Test_runtime.list_tests

let test = Test_runtime.test

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

type bench_event = Bench_runtime.bench_event =
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

type bench_error = Bench_runtime.bench_error =
  | BuildFailed of build_error
  | ClientError of Client.error
  | SuiteArtifactNotFound of { suite: suite_binary; reason: string }
  | SuiteExecutionError of { suite: suite_binary; reason: string }
  | SuitesFailed of int

let collect_bench_suites = Bench_runtime.collect_suite_binaries

let bench_error_message = Bench_runtime.bench_error_message

let bench_event_to_json = Bench_runtime.bench_event_to_json

let list_benchmarks = Bench_runtime.list_benchmarks

let bench = Bench_runtime.bench

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

type install_event = Install_runtime.install_event =
  | Build of build_event
  | InstallingBinary of { package: string; binary: string }
  | PromotedBinary of { binary: string; destination: Path.t; global: bool }
  | InstalledBinary of { binary: string; duration_ms: int; global_destination: Path.t option }

type install_error = Install_runtime.install_error =
  | BinaryNotFound of { binary_name: string }
  | BinaryNotFoundInPackage of { package_name: string; binary_name: string }
  | BuildFailed of build_error
  | ArtifactNotFound of { package_name: string; binary_name: string; reason: string }
  | PromotionFailed of { binary_name: string; destination: Path.t; global: bool; reason: string }
  | ExternalTargetLoadFailed of { target: string; reason: string }
  | ClientError of Client.error

let install_error_message = Install_runtime.install_error_message

let install_event_to_json = Install_runtime.install_event_to_json

let install = Install_runtime.install

let install_source = Install_runtime.install_source

let install_registry = Install_runtime.install_registry
