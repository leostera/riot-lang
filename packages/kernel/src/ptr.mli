(** Physical equality comparisons backed by OCaml runtime primitives. *)
type 'value t = 'value
val eq: 'value -> 'value -> bool

val not_eq: 'value -> 'value -> bool
