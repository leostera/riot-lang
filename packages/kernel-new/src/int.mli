type t = int
val zero: t

val one: t

val add: t -> t -> t

val sub: t -> t -> t

val mul: t -> t -> t

val div: t -> t -> t

val rem: t -> t -> t

val equal: t -> t -> bool

val compare: t -> t -> int

val hash: t -> int

(** Use `to_string value` to render `value` in decimal. *)
val to_string: t -> string
