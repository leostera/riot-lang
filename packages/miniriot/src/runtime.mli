(** Runtime support for Riot - provides reduction counting for compiler
    instrumentation *)

val reset_reductions : int -> unit
(** Reset the reduction count to a new value *)

val increment_reduction_count : unit -> unit
(** Increment (actually decrement) the reduction count and yield if necessary.
    This function is automatically injected by the Riot-patched OCaml compiler
    at function applications and loop iterations. *)
