(** RFC 4648 Base64 encoding and decoding. *)
open Global

val encode: string -> string

val encode_bytes: bytes -> string

val decode: string -> (string, [
    `Invalid_base64
  ]) result
