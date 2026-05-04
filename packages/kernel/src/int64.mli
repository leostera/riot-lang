type t = int64

val zero: t

val min_int: t

val max_int: t

(** Use `from_int value` for explicit widening into `Int64`. *)
val from_int: int -> t

(** Use `from_int value` as the conventional alias for `from_int value`. *)
val from_int: int -> t

(** Use `to_int value` for explicit narrowing back into `Int`. *)
val to_int: t -> int

val logand: t -> t -> t

val logor: t -> t -> t

val logxor: t -> t -> t

val lognot: t -> t

val shift_left: t -> int -> t

val shift_right: t -> int -> t

val shift_right_logical: t -> int -> t

val abs: t -> t

val neg: t -> t

val add: t -> t -> t

val sub: t -> t -> t

val mul: t -> t -> t

val div: t -> t -> t

val rem: t -> t -> t

val succ: t -> t

val pred: t -> t

val from_float: float -> t

val to_float: t -> float

val bits_of_float: float -> t

val float_of_bits: t -> float

val from_int32: int32 -> t

val from_int32: int32 -> t

val to_int32: t -> int32

val parse_unchecked: string -> t

val from_string: string -> t

val from_string_opt: string -> t option

val parse: string -> t option

val to_string: t -> string

val hash: t -> int

val equal: t -> t -> bool

val compare: t -> t -> Order.t
