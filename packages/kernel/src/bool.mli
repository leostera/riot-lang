type t = bool
val true_: t

val false_: t

val equal: t -> t -> bool

val compare: t -> t -> int

(** Use `not value` to invert a boolean. *)
val not: t -> bool

(** Use `to_string value` for the stable lowercase forms `"true"` and `"false"`. *)
val to_string: t -> string
