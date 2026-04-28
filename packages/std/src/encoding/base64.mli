(** RFC 4648 Base64 encoding and decoding. *)
open Global

type decode_error =
  | InvalidBase64
val encode: string -> string

val encode_bytes: bytes -> string

val decode: string -> (string, decode_error) result
