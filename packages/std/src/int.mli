type t = int

val zero: t

val one: t

val add: t -> t -> t

val sub: t -> t -> t

val mul: t -> t -> t

val div: t -> t -> t

val rem: t -> t -> t

val max_int: t

val min_int: t

val equal: t -> t -> bool

val compare: t -> t -> Order.t

val abs: t -> t

val min: t -> t -> t

val max: t -> t -> t

val succ: t -> t

val pred: t -> t

val from_float: float -> t

val from_string: string -> t

val from_string_opt: string -> t option

val parse: string -> t option

val parse_unchecked: string -> t

val hash: t -> int

(** Use `to_string value` to render `value` in decimal. *)
val to_string: t -> string

val ( = ): t -> t -> bool

val ( != ): t -> t -> bool

val ( < ): t -> t -> bool

val ( > ): t -> t -> bool

val ( <= ): t -> t -> bool

val ( >= ): t -> t -> bool

val ( + ): t -> t -> t

val ( - ): t -> t -> t

val ( * ): t -> t -> t

val ( / ): t -> t -> t

val ( mod ): t -> t -> t
