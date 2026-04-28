(** RFC 4648 Base32 encoding and decoding. *)
open Global

type decode_error =
  | InvalidBase32
val encode: string -> string

val encode_bytes: bytes -> string

val decode: string -> (string, decode_error) result

val decode_bytes: string -> (bytes, decode_error) result
