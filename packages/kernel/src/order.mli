type t =
  | LT
  | EQ
  | GT

val compare: 'value -> 'value -> t
