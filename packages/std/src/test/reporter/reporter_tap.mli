(** TAP reporter for test runs. *)
val init: Intf.suite_info -> int -> unit

(** Accumulate one completed test result. *)
val on_result: int -> Test_result.t -> unit

(** Emit the final TAP summary. *)
val finalize: Test_result.summary -> unit
