(** Ascii85/Base85 encoding and decoding. *)
open Global

type decode_error =
  | InvalidBase85
val encode: string -> string

val encode_bytes: bytes -> string

val decode: string -> (string, decode_error) result

val decode_bytes: string -> (bytes, decode_error) result
