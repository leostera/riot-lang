type t = float

(** Use `equal left right` for the raw runtime float equality semantics. *)
val equal: t -> t -> bool

(** Use `compare left right` for the runtime float ordering semantics. *)
val compare: t -> t -> int

val of_int: int -> t

val to_int: t -> int

val to_string: ?precision:int -> t -> string

val rem: t -> t -> t

val sqrt: t -> t

val floor: t -> t

val ceil: t -> t

val pow: t -> t -> t

val round: float -> float
