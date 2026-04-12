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

val min: t -> t -> t

val max: t -> t -> t

val succ: t -> t

val pred: t -> t

val of_float: float -> t

val parse: string -> t

val parse_opt: string -> t option

val of_string: string -> t

val of_string_opt: string -> t option

val hash: t -> int

(** Use `to_string value` to render `value` in decimal. *)
val to_string: t -> string
