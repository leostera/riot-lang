open Std

type t
type error =
  | Empty
  | InvalidLeadingCharacter of { value: string; suggestion: string }
  | TrailingDelimiter of { value: string }
  | InvalidCharacterSet of { value: string }
val error_message: error -> string

val from_string: string -> (t, error) result

val to_string: t -> string

val equal: t -> t -> bool

val compare: t -> t -> int
