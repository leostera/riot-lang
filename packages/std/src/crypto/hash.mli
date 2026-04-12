type t
val of_bytes: bytes -> t

val to_bytes: t -> bytes

val length: t -> int

val equal: t -> t -> bool

val compare: t -> t -> int
