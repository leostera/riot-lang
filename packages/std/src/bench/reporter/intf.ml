type suite_info = { name: string }

module type Intf = sig
  val init: suite_info -> int -> unit

  val on_case_start: int -> string -> iterations:int -> warmup:int -> unit

  val on_result: int -> Bench_result.t -> unit

  val finalize: Bench_result.summary -> unit

  (* Comparison reporting *)

  (* Comparison reporting *)
  val on_comparison_start: int -> string -> int -> unit

  val on_comparison_case_result: int -> string -> Bench_result.statistics -> unit

  val on_comparison_summary: Bench_result.comparison_result -> unit
end
