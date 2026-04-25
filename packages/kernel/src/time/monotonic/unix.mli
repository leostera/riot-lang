type t

type error =
  | InvalidNanoseconds of { nanos: int }
  | System of System_error.t

val error_to_string: error -> string

val to_parts: t -> int * int

val secs: t -> int

val subsec_nanos: t -> int

val now: unit -> (t, error) Result.t

val compare: t -> t -> Order.t

val equal: t -> t -> bool

val diff_ns: t -> t -> int64
