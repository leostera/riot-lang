open Global

(** Configuration for running benchmarks. *)
(** Summary of benchmark run. *)
type config = {
  reporter: (module Reporter.Intf.Intf);
  suite_info: Reporter.Intf.suite_info;
}
(** A benchmark item - either a single benchmark or a comparison. *)
type run_summary = Bench_result.summary
(** [run_benchmarks ~config benchmarks] runs all benchmarks (single and comparison) and returns a summary. *)
type bench_item =
  | Single of Bench_case.t
  | Compare of Bench_comparison.t
val run_benchmarks: config:config -> bench_item list -> run_summary
