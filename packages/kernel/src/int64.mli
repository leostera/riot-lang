type t = int64

(** Use `of_int value` for explicit widening into `Int64`. *)
val of_int: int -> t

(** Use `to_int value` for explicit narrowing back into `Int`. *)
val to_int: t -> int

val neg: t -> t

val add: t -> t -> t

val sub: t -> t -> t

val mul: t -> t -> t

val div: t -> t -> t

val rem: t -> t -> t

val succ: t -> t

val pred: t -> t

val of_float: float -> t

val to_float: t -> float

val of_int32: int32 -> t

val hash: t -> int

val equal: t -> t -> bool

val compare: t -> t -> int
