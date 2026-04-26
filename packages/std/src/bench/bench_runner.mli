open Global

(** Configuration for running benchmarks. *)
type config = {
  (** Reporter implementation used to display progress and results. *)
  reporter: (module Reporter.Intf.Intf);
  (** Metadata describing the benchmark suite being executed. *)
  suite_info: Reporter.Intf.suite_info;
}
(** Summary of a completed benchmark run. *)
type run_summary = Bench_result.summary
(** A benchmark item, either a single benchmark or a comparison. *)
type bench_item =
  | Single of Bench_case.t
  | Compare of Bench_comparison.t

(**
   [run_benchmarks ~config benchmarks] runs all benchmarks and returns a
   summary.

   ## Example

   ```ocaml
   let config =
     Bench_runner.
       {
         reporter = (module Bench.Reporter.Default);
         suite_info = { Bench.Reporter.Intf.name = "Collections" };
       }
   in
   let summary = Bench_runner.run_benchmarks ~config benchmarks in
   ignore summary
   ```
*)
val run_benchmarks: config:config -> bench_item list -> run_summary
