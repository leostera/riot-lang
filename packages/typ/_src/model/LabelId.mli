open Std

type t

val compare: t -> t -> int

val equal: t -> t -> bool

val of_int: int -> t

val to_int: t -> int

val to_string: t -> string
