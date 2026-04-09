type t = char
val equal: t -> t -> bool

val compare: t -> t -> int

val of_int: int -> t

val unsafe_of_int: int -> t

val to_int: t -> int
