open Std

type suite_binary = {
  package_name: string;
  suite_name: string;
}
type test_request = {
  workspace: Tusk_model.Workspace.t;
  package_filter: string option;
  query: string option;
  extra_args: string list;
}
type test_event =
  | Build of Build_runtime.build_event
  | NoSuitesFound of { package_name: string option }
  | RunningSuite of suite_binary
  | SuiteCompleted of { suite: suite_binary; status: int; stdout: string; stderr: string }
  | Summary of { total: int; passed: int; failed: int }
type test_error =
  | BuildFailed of Build_runtime.build_error
  | ClientError of Client.error
  | SuiteArtifactNotFound of { suite: suite_binary; reason: string }
  | SuiteExecutionError of { suite: suite_binary; reason: string }
  | SuitesFailed of int
val collect_suite_binaries:
  Tusk_model.Workspace.t -> ?package_filter:string -> unit -> suite_binary list

val test_error_message: test_error -> string

val test_event_to_json: test_event -> Data.Json.t option

val test: ?on_event:(test_event -> unit) -> test_request -> (unit, test_error) result
