(** Hexadecimal encoding and decoding. *)
open Global

type decode_error =
  | InvalidBase16

val encode: string -> string

val encode_lower: string -> string

val encode_bytes: bytes -> string

val encode_bytes_lower: bytes -> string

val decode: string -> (string, decode_error) result

val decode_bytes: string -> (bytes, decode_error) result
