type t
type error =
  | Invalid_nanoseconds of { nanos: int }
  | System of System_error.t
val error_to_string: error -> string

val epoch: t

val of_parts: secs:int -> nanos:int -> (t, error) Result.t

val to_parts: t -> int * int

val secs: t -> int

val subsec_nanos: t -> int

val now: unit -> (t, error) Result.t

val compare: t -> t -> int

val equal: t -> t -> bool

val diff_ns: t -> t -> int64
