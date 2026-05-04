type t = int32

val zero: t

val min_int: t

val max_int: t

(** Use `from_int value` for explicit narrowing or widening into `Int32`. *)
val from_int: int -> t

(** Use `from_int value` as the conventional alias for `from_int value`. *)
val from_int: int -> t

(** Use `to_int value` for explicit conversion back into `Int`. *)
val to_int: t -> int

val neg: t -> t

val abs: t -> t

val add: t -> t -> t

val sub: t -> t -> t

val mul: t -> t -> t

val div: t -> t -> t

val rem: t -> t -> t

(** Use `logand left right` for bitwise masking on 32-bit integers. *)
val logand: t -> t -> t

(** Use `logor left right` for bitwise union on 32-bit integers. *)
val logor: t -> t -> t

val logxor: t -> t -> t

val shift_left: t -> int -> t

val shift_right: t -> int -> t

val shift_right_logical: t -> int -> t

val from_float: float -> t

val to_float: t -> float

val bits_of_float: float -> t

val float_of_bits: t -> float

(** Use `from_string value` to parse a textual 32-bit integer. *)
val parse_unchecked: string -> t

val from_string: string -> t

val from_string_opt: string -> t option

val parse: string -> t option

(** Use `to_string value` to render `value` in signed decimal. *)
val to_string: t -> string

(** Use `equal left right` for exact `Int32` equality. *)
val equal: t -> t -> bool

(** Use `compare left right` for `Int32` ordering. *)
val compare: t -> t -> Order.t
