open Global

type config = {
  reporter : (module Reporter.Intf.Intf);
  suite_info : Reporter.Intf.suite_info;
}
(** Configuration for running benchmarks. *)

type run_summary = Bench_result.summary
(** Summary of benchmark run. *)

type bench_item = 
  | Single of Bench_case.t
  | Compare of Bench_comparison.t
(** A benchmark item - either a single benchmark or a comparison. *)

val run_benchmarks : config:config -> bench_item list -> run_summary
(** [run_benchmarks ~config benchmarks] runs all benchmarks (single and comparison) and returns a summary. *)
