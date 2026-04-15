open Std

type t
val from_string: string -> (t, string) result

val to_string: t -> string

val equal: t -> t -> bool

val compare: t -> t -> int
