type t = int64

(** Use `of_int value` for explicit widening into `Int64`. *)
val of_int: int -> t

(** Use `to_int value` for explicit narrowing back into `Int`. *)
val to_int: t -> int

val add: t -> t -> t

val mul: t -> t -> t

val div: t -> t -> t

val rem: t -> t -> t

val equal: t -> t -> bool

val compare: t -> t -> int
