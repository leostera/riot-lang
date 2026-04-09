type t
val make: 'value -> t

val unsafe_value: t -> 'value

val id: t -> int

val hash: t -> int

val equal: t -> t -> bool
