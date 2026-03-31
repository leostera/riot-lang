open Global

(** A single timing measurement. *)
(** Statistical summary of benchmark timings. *)
type timing = {
  iteration: int;
  duration: Time.Duration.t;
}
(** The result of running a benchmark. *)
type statistics = {
  min: Time.Duration.t;
  max: Time.Duration.t;
  mean: Time.Duration.t;
  median: Time.Duration.t;
  std_dev: Time.Duration.t;
  iterations: int;
  total_time: Time.Duration.t;
}
(** A benchmark result with its index and name. *)
type bench_result =
  | Completed of statistics
  | Failed of string
  | Skipped
(** [make_statistics timings] computes statistics from a list of timings. *)
type t = {
  index: int;
  name: string;
  result: bench_result;
}
val make_statistics: timing list -> statistics

(** Summary of all benchmark results. *)
(** [make_summary results] creates a summary from benchmark results. *)
type summary = {
  total: int;
  completed: int;
  skipped: int;
  failed: int;
}
val make_summary: t list -> summary

(** {1 Comparison Results} *)

(** Result of a single case in a comparison benchmark. *)
(** Result of a comparison benchmark showing relative performance. *)
type case_result = {
  name: string;
  statistics: statistics;
}
(** [make_comparison_result description case_results] creates a comparison result,
    identifying the fastest case and calculating speedup ratios. *)
type comparison_result = {
  description: string;
  case_results: case_result list;
  fastest: string;
  speedup_ratios: (string * float) list;
}
val make_comparison_result: string -> case_result list -> comparison_result
