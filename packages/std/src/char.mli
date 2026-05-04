type t = char

val equal: t -> t -> bool

val compare: t -> t -> Order.t

(**
   Use `from_int value` to build a byte-sized character only when `value` is in the inclusive
   range `0` to `255`.
*)
val from_int: int -> t option

(**
   Use `unsafe_of_int value` only when the caller already knows `value` is in the inclusive
   range `0` to `255`.
*)
val from_int_unchecked: int -> t

(** Use `to_int value` to recover the byte value in the inclusive range `0` to `255`. *)
val to_int: t -> int

(** Use `code value` as the conventional alias for `to_int value`. *)
val code: t -> int

val lowercase_ascii: t -> t

val uppercase_ascii: t -> t
