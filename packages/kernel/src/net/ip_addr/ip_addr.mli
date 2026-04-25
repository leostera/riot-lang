type t

type error =
  | InvalidText of { value: string }

val error_to_string: error -> string

val v4_loopback: t

val v6_loopback: t

(**
   Use `from_string text` to validate a textual IP literal immediately.

   It performs no name resolution and does not touch the network. 
*)
val from_string: string -> (t, error) Result.t

val to_string: t -> string

val compare: t -> t -> Order.t

val equal: t -> t -> bool
