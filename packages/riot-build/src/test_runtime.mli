open Std

type suite_binary = {
  package_name: string;
  suite_name: string;
}
type test_request = {
  workspace: Riot_model.Workspace.t;
  package_filter: string option;
  suite_filter: string option;
  profile: string;
  extra_args: string list;
}
type test_case_type =
  | Test
  | Property of { examples: int }
type test_case_size =
  | Small
  | Large
type test_case_reliability =
  | Stable
  | Flaky of { retry_attempts: int }
type test_case_status =
  | Passed
  | Failed of string
  | Timed_out of { timeout_ms: int }
  | Skipped
type test_case_result = {
  index: int;
  name: string;
  test_type: test_case_type;
  size: test_case_size;
  reliability: test_case_reliability;
  attempts: int;
  result: test_case_status;
  duration_us: int;
}
type failed_test = {
  suite: suite_binary;
  name: string;
  message: string;
  duration_us: int;
}
type test_suite_summary = {
  total: int;
  passed: int;
  failed: int;
  skipped: int;
  duration_us: int;
  results: test_case_result list;
}
type test_event =
  | Build of Build_runtime.build_event
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
  | BuildFailed of Build_runtime.build_error
  | ClientError of Client.error
  | SuiteArtifactNotFound of { suite: suite_binary; reason: string }
  | SuiteExecutionError of { suite: suite_binary; reason: string }
  | SuitesFailed of int
val collect_suite_binaries:
  Riot_model.Workspace.t -> ?package_filter:string -> ?suite_filter:string -> unit -> suite_binary list

val test_error_message: test_error -> string

val test_event_to_json: test_event -> Data.Json.t option

val test: ?on_event:(test_event -> unit) -> test_request -> (unit, test_error) result
