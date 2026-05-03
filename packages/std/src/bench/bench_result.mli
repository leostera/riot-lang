open Global

(** A single timing measurement. *)
type timing = {
  (** Iteration number for this measurement. *)
  iteration: int;
  (** Measured duration for the iteration. *)
  duration: Time.Duration.t;
}
(** GC collection counters observed while measuring a benchmark case. *)
type gc_stats = Kernel.Gc.quick_stat = {
  minor_collections: int;
  major_collections: int;
  compactions: int;
}
(** Statistical summary of benchmark timings. *)
type statistics = {
  (** Fastest recorded duration. *)
  min: Time.Duration.t;
  (** Slowest recorded duration. *)
  max: Time.Duration.t;
  (** Arithmetic mean duration. *)
  mean: Time.Duration.t;
  (** Median duration across all iterations. *)
  median: Time.Duration.t;
  (** Standard deviation of the timing distribution. *)
  std_dev: Time.Duration.t;
  (** Number of measured iterations. *)
  iterations: int;
  (** Total duration across all iterations. *)
  total_time: Time.Duration.t;
  (** GC collection deltas observed across the measured iterations. *)
  gc: gc_stats;
}
(** The outcome of running a benchmark. *)
type bench_result =
  | Completed of statistics
  | Failed of string
  | Skipped
(** A benchmark result tagged with its index and name. *)
type t = {
  (** Position of the benchmark in the run. *)
  index: int;
  (** Human-readable benchmark name. *)
  name: string;
  (** Outcome for this benchmark. *)
  result: bench_result;
}

(**
   [make_statistics timings] computes statistical aggregates from a list of
   timing samples.

   ## Example

   ```ocaml
   let stats = Bench_result.make_statistics timings
   ```
*)
val make_statistics: ?gc:gc_stats -> timing list -> statistics

(** Summary of all benchmark results. *)
type summary = {
  (** Total number of benchmarks considered. *)
  total: int;
  (** Number of benchmarks that completed successfully. *)
  completed: int;
  (** Number of skipped benchmarks. *)
  skipped: int;
  (** Number of failed benchmarks. *)
  failed: int;
}

(**
   [make_summary results] creates a run summary from benchmark results.

   ## Example

   ```ocaml
   let summary = Bench_result.make_summary results
   ```
*)
val make_summary: t list -> summary

(** Result of a single case in a comparison benchmark. *)
type case_result = {
  (** Case name. *)
  name: string;
  (** Measured statistics for that case. *)
  statistics: statistics;
}
(** Result of a comparison benchmark showing relative performance. *)
type comparison_result = {
  (** Human-readable comparison description. *)
  description: string;
  (** Results for each compared case. *)
  case_results: case_result list;
  (** Name of the fastest case. *)
  fastest: string;
  (** Relative speedups keyed by case name. *)
  speedup_ratios: (string * float) list;
}

(**
   [make_comparison_result description case_results] creates a comparison
   result, identifies the fastest case, and calculates speedup ratios.

   ## Example

   ```ocaml
   let comparison = Bench_result.make_comparison_result "insert" case_results
   ```
*)
val make_comparison_result: string -> case_result list -> comparison_result
