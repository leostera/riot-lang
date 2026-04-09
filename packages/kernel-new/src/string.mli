type t = string
val empty: t

val length: t -> int

val get: t -> int -> char

val init: int -> (int -> char) -> t

val make: int -> char -> t

val append: t -> t -> t

val concat: t -> t list -> t

val equal: t -> t -> bool

val compare: t -> t -> int

val of_bytes: bytes -> t

val to_bytes: t -> bytes
