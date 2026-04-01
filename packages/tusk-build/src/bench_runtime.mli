open Std

type suite_binary = Test_runtime.suite_binary = {
  package_name: string;
  suite_name: string;
}
type bench_request = {
  workspace: Tusk_model.Workspace.t;
  package_filter: string option;
  query: string option;
  extra_args: string list;
}
type bench_event =
  | Build of Build_runtime.build_event
  | NoSuitesFound of { package_name: string option }
  | RunningSuite of suite_binary
  | SuiteCompleted of { suite: suite_binary; status: int; stdout: string; stderr: string }
  | Summary of { total: int; passed: int; failed: int }
type bench_error =
  | BuildFailed of Build_runtime.build_error
  | ClientError of Client.error
  | SuiteArtifactNotFound of { suite: suite_binary; reason: string }
  | SuiteExecutionError of { suite: suite_binary; reason: string }
  | SuitesFailed of int
val collect_suite_binaries:
  Tusk_model.Workspace.t -> ?package_filter:string -> unit -> suite_binary list

val bench_error_message: bench_error -> string

val bench_event_to_json: bench_event -> Data.Json.t option

val bench: ?on_event:(bench_event -> unit) -> bench_request -> (unit, bench_error) result
