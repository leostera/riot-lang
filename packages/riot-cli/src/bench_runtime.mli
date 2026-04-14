open Std
open Riot_model

(** Build-time benchmarking support used by [riot bench].

    This module discovers benchmark suites, runs them through the shared build
    pipeline, and emits structured per-suite and final summary events.
*)
type suite_binary = Test_runtime.suite_binary = {
  (** Package that owns the benchmark suite binary. *)
  package_name: Package_name.t;
  (** Suite name reported by the benchmark binary. *)
  suite_name: string;
}
type bench_request = {
  (** Workspace providing the packages and build configuration. *)
  workspace: Workspace.t;
  (** Optional package filter narrowing which suites should run. *)
  package_filter: Package_name.t option;
  (** Optional suite filter narrowing which benchmark binary should run. *)
  suite_filter: string option;
  (** Build profile used for the benchmark run. *)
  profile: string;
  (** Additional CLI arguments forwarded to each suite binary. *)
  extra_args: string list;
}
type bench_statistics = {
  (** Fastest measured iteration. *)
  min: Time.Duration.t;
  (** Slowest measured iteration. *)
  max: Time.Duration.t;
  (** Arithmetic mean across iterations. *)
  mean: Time.Duration.t;
  (** Median iteration time. *)
  median: Time.Duration.t;
  (** Standard deviation across iterations. *)
  std_dev: Time.Duration.t;
  (** Number of benchmark iterations measured. *)
  iterations: int;
  (** Total wall-clock time spent benchmarking the case. *)
  total_time: Time.Duration.t;
}
type bench_case_status =
  | Completed of bench_statistics
  | Failed of string
  | Skipped
type bench_case_result = {
  (** Case index within the suite output. *)
  index: int;
  (** Human-readable benchmark case name. *)
  name: string;
  (** Final result for the case. *)
  result: bench_case_status;
}
type listed_bench_item_kind =
  | Benchmark
  | Comparison
type listed_bench_item = {
  index: int;
  name: string;
  kind: listed_bench_item_kind;
  iterations: int;
  warmup: int;
  skip: bool;
  cases: string list;
}
type listed_bench_suite = {
  suite: suite_binary;
  source_path: Path.t option;
  benchmarks: listed_bench_item list;
}
type bench_comparison_case_result = {
  (** Name of a case participating in the comparison. *)
  name: string;
  (** Timing statistics for that case. *)
  statistics: bench_statistics;
}
type bench_comparison_result = {
  (** Description reported by the benchmark suite. *)
  description: string;
  (** Per-case statistics included in the comparison. *)
  case_results: bench_comparison_case_result list;
  (** Name of the fastest case in the comparison. *)
  fastest: string;
  (** Relative speedup ratios keyed by case name. *)
  speedup_ratios: (string * float) list;
}
type bench_suite_summary = {
  (** Number of benchmark cases seen in the suite. *)
  total: int;
  (** Number of completed benchmark cases. *)
  completed: int;
  (** Number of skipped benchmark cases. *)
  skipped: int;
  (** Number of failed benchmark cases. *)
  failed: int;
}
type bench_event =
  | Build of Riot_build.Event.t
  | NoSuitesFound of { package_name: Package_name.t option }
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
  | BuildFailed of Riot_build.error
  | SuiteArtifactNotFound of { suite: suite_binary; reason: string }
  | SuiteExecutionError of { suite: suite_binary; reason: string }
  | SuitesFailed of int

(** Collect benchmark suites available in the workspace.

    Use [package_filter] to restrict discovery to one package.
*)
val collect_suite_binaries:
  Workspace.t ->
  ?package_filter:Package_name.t ->
  ?suite_filter:string ->
  unit ->
  suite_binary list

(** Render a user-facing error message for a benchmark failure. *)
val bench_error_message: bench_error -> string

(** Convert a benchmark event into JSON when it has a machine-readable form. *)
val bench_event_to_json: bench_event -> Data.Json.t option

val list_benchmarks:
  ?on_suite:(listed_bench_suite -> unit) ->
  ?on_suite_error:(suite_binary -> bench_error -> unit) ->
  bench_request ->
  (listed_bench_suite list, bench_error) result

(** Build and run benchmark suites for the given request.

    Use [on_event] to stream progress and per-suite results to a CLI or other
    consumer while the run is in progress.
*)
val bench: ?on_event:(bench_event -> unit) -> bench_request -> (unit, bench_error) result
