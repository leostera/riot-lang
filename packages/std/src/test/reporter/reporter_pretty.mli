(** Pretty terminal reporter for test runs. *)
val init: Intf.suite_info -> int -> unit

(** Report one completed test result. *)
val on_result: int -> Test_result.t -> unit

(** Report a non-fatal suite-level warning. *)
val warn: string -> unit

(** Print the final test summary. *)
val finalize: Test_result.summary -> unit
