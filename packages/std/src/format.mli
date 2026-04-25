(** Mechanical string assembly helpers for `std`. *)
type t =
  | String of string
  | Char of char
  | Bool of bool
  | Int of int
  | Bytes of bytes

val str: string -> t

val char: char -> t

val bool: bool -> t

val int: int -> t

val bytes: bytes -> t

val to_string: t -> string

val format: t list -> string
