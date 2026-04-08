type t
type error = Error.t
val v4_loopback: t

val v6_loopback: t

val of_string: string -> (t, error) Result.t

val to_string: t -> string

val compare: t -> t -> int

val equal: t -> t -> bool
