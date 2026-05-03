(**
   Boolean helpers.

   Re-export of the core boolean helpers from [Kernel.Bool].
*)
type t = bool

val equal: t -> t -> bool

val compare: t -> t -> Order.t

(** Use `not value` to invert a boolean. *)
val not: t -> bool

(** Use `to_string value` for the stable lowercase forms `"true"` and `"false"`. *)
val to_string: t -> string
