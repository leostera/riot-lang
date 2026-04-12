type t
type error =
  | InvalidText of { value: string }
val error_to_string: error -> string

val v4_loopback: t

val v6_loopback: t

(** Use `of_string text` to validate a textual IP literal immediately.

    It performs no name resolution and does not touch the network. *)
val of_string: string -> (t, error) Result.t

val to_string: t -> string

val compare: t -> t -> int

val equal: t -> t -> bool
