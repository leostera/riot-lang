open Std

type error =
  | Empty
  | Empty_part
  | Invalid_part of string

type t

val from_parts: string list -> (t, error) result

val error_message: error -> string

val to_string: t -> string

val parts: t -> string list
