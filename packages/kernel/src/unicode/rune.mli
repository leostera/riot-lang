open Prelude

type t = int [@@ immediate]
type utf_decode = int [@@ immediate]
type error =
  | BadRune of { int: int }
val min: t

val max: t

val replacement: t

val max_ascii: t

val max_latin1: t

val is_valid: int -> bool

val from_int: int -> (t, error) result

val from_int_unchecked: int -> t

val to_int: t -> int

val from_char: char -> t

val to_char: t -> char

val equal: t -> t -> bool

val compare: t -> t -> Order.t

val utf_decode_is_valid: utf_decode -> bool

val utf_decode_rune: utf_decode -> t

val utf_decode_length: utf_decode -> int

val utf_decode: int -> t -> utf_decode

val utf_decode_invalid: int -> utf_decode

val utf_8_byte_length: t -> int

val to_string: t -> string
