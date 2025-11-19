open Global

type timing = { iteration : int; duration : Time.Duration.t }
(** A single timing measurement. *)

type statistics = {
  min : Time.Duration.t;
  max : Time.Duration.t;
  mean : Time.Duration.t;
  median : Time.Duration.t;
  std_dev : Time.Duration.t;
  iterations : int;
  total_time : Time.Duration.t;
}
(** Statistical summary of benchmark timings. *)

type bench_result = Completed of statistics | Failed of string | Skipped
(** The result of running a benchmark. *)

type t = { index : int; name : string; result : bench_result }
(** A benchmark result with its index and name. *)

val make_statistics : timing list -> statistics
(** [make_statistics timings] computes statistics from a list of timings. *)

type summary = { total : int; completed : int; skipped : int; failed : int }
(** Summary of all benchmark results. *)

val make_summary : t list -> summary
(** [make_summary results] creates a summary from benchmark results. *)

(** {1 Comparison Results} *)

type case_result = { name : string; statistics : statistics }
(** Result of a single case in a comparison benchmark. *)

type comparison_result = {
  description : string;
  case_results : case_result list;
  fastest : string;
  speedup_ratios : (string * float) list;
}
(** Result of a comparison benchmark showing relative performance. *)

val make_comparison_result : string -> case_result list -> comparison_result
(** [make_comparison_result description case_results] creates a comparison result,
    identifying the fastest case and calculating speedup ratios. *)
