type t
type error = Error.t
val epoch: t

val of_parts: secs:int -> nanos:int -> t

val to_parts: t -> int * int

val secs: t -> int

val subsec_nanos: t -> int

val now: unit -> (t, error) Result.t

val compare: t -> t -> int

val equal: t -> t -> bool
