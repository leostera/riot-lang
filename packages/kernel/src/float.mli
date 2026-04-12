type t = float
val max_float: t

val min_float: t

(** Use `equal left right` for the raw runtime float equality semantics. *)
val equal: t -> t -> bool

(** Use `compare left right` for the runtime float ordering semantics. *)
val compare: t -> t -> int

val of_int: int -> t

val to_int: t -> int

val parse: string -> t

val parse_opt: string -> t option

val of_string: string -> t

val of_string_opt: string -> t option

val is_finite: t -> bool

val is_infinite: t -> bool

val is_nan: t -> bool

val to_string: ?precision:int -> t -> string

val rem: t -> t -> t

val abs: t -> t

val sqrt: t -> t

val cbrt: t -> t

val floor: t -> t

val ceil: t -> t

val pow: t -> t -> t

val round: float -> float
