(** Metadata describing a benchmark suite. *)
type suite_info = {
  (** Human-readable suite name. *)
  name: string;
}

(**
   Reporter interface used by the benchmark runner.

   ## Example

   ```ocaml
   module Reporter : Bench.Reporter.Intf.Intf = struct
     let init suite_info total =
       Log.info "running %d benchmarks for %s" total suite_info.name

     let on_result _index _result = ()

     let finalize _summary = ()

     let on_comparison_start _index _description _count = ()

     let on_comparison_case_result _index _name _stats = ()

     let on_comparison_summary _summary = ()
   end
   ```
*)
module type Intf = sig
  (**
     Called once before any benchmarks are executed.

     The integer argument is the total number of benchmark items.
  *)
  val init: suite_info -> int -> unit

  (**
     Called when a benchmark case starts running.

     The arguments are the outer benchmark index, the case name, the measured
     iteration count, and the warmup count. Comparison cases reuse the outer
     comparison index and report the individual case name.
  *)
  val on_case_start: int -> string -> iterations:int -> warmup:int -> unit

  (** Called when a single benchmark finishes. *)
  val on_result: int -> Bench_result.t -> unit

  (** Called once after all benchmarks have completed. *)
  val finalize: Bench_result.summary -> unit

  (**
     Called before running a comparison benchmark.

     The arguments are the benchmark index, its description, and the number of
     compared cases.
  *)
  val on_comparison_start: int -> string -> int -> unit

  (** Called when one case in a comparison benchmark finishes. *)
  val on_comparison_case_result: int -> string -> Bench_result.statistics -> unit

  (** Called when a comparison benchmark has been fully summarized. *)
  val on_comparison_summary: Bench_result.comparison_result -> unit
end
