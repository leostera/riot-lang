open Std

type suite_binary = Test_runtime.suite_binary = {
  package_name: string;
  suite_name: string;
}
type bench_request = {
  workspace: Riot_model.Workspace.t;
  package_filter: string option;
  query: string option;
  extra_args: string list;
}
type bench_statistics = {
  min: Time.Duration.t;
  max: Time.Duration.t;
  mean: Time.Duration.t;
  median: Time.Duration.t;
  std_dev: Time.Duration.t;
  iterations: int;
  total_time: Time.Duration.t;
}
type bench_case_status =
  | Completed of bench_statistics
  | Failed of string
  | Skipped
type bench_case_result = {
  index: int;
  name: string;
  result: bench_case_status;
}
type bench_comparison_case_result = {
  name: string;
  statistics: bench_statistics;
}
type bench_comparison_result = {
  description: string;
  case_results: bench_comparison_case_result list;
  fastest: string;
  speedup_ratios: (string * float) list;
}
type bench_suite_summary = {
  total: int;
  completed: int;
  skipped: int;
  failed: int;
}
type bench_event =
  | Build of Build_runtime.build_event
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
  | BuildFailed of Build_runtime.build_error
  | ClientError of Client.error
  | SuiteArtifactNotFound of { suite: suite_binary; reason: string }
  | SuiteExecutionError of { suite: suite_binary; reason: string }
  | SuitesFailed of int
val collect_suite_binaries:
  Riot_model.Workspace.t -> ?package_filter:string -> unit -> suite_binary list

val bench_error_message: bench_error -> string

val bench_event_to_json: bench_event -> Data.Json.t option

val bench: ?on_event:(bench_event -> unit) -> bench_request -> (unit, bench_error) result
