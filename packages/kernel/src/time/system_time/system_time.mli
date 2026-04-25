type t
type error =
  | InvalidNanoseconds of { nanos: int }
  | System of System_error.t
val error_to_string: error -> string

val epoch: t

val from_parts: secs:int -> nanos:int -> (t, error) Result.t

val to_parts: t -> int * int

val secs: t -> int

val subsec_nanos: t -> int

(**
   Use `now ()` for an immediate clock read.

   It does not participate in readiness waiting and intentionally stays synchronous.
*)
val now: unit -> (t, error) Result.t

val compare: t -> t -> Order.t

val equal: t -> t -> bool

val diff_ns: t -> t -> int64
