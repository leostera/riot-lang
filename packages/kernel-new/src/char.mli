type t = char
val equal: t -> t -> bool

val compare: t -> t -> int

(** Use `of_int value` to build a byte-sized character only when `value` is in the inclusive
    range `0` to `255`. *)
val of_int: int -> t option

(** Use `unsafe_of_int value` only when the caller already knows `value` is in the inclusive
    range `0` to `255`. *)
val unsafe_of_int: int -> t

(** Use `to_int value` to recover the byte value in the inclusive range `0` to `255`. *)
val to_int: t -> int
