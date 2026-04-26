(** Physical equality comparisons backed by OCaml runtime primitives. *)
type 'value t = 'value
val equal: 'value -> 'value -> bool

val not_equal: 'value -> 'value -> bool
