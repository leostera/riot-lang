(** Default terminal reporter for benchmark runs. *)
val init: Intf.suite_info -> int -> unit

(** Report one completed benchmark result. *)
val on_result: int -> Bench_result.t -> unit

(** Print the final summary for a benchmark run. *)
val finalize: Bench_result.summary -> unit

(** Announce the start of a comparison benchmark. *)
val on_comparison_start: int -> string -> int -> unit

(** Report one completed case inside a comparison benchmark. *)
val on_comparison_case_result: int -> string -> Bench_result.statistics -> unit

(** Print the summary for a comparison benchmark. *)
val on_comparison_summary: Bench_result.comparison_result -> unit
