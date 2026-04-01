(** RFC 4648 Base32 encoding and decoding. *)
open Global

val encode: string -> string

val encode_bytes: bytes -> string

val decode: string -> (string, [
    `Invalid_base32
  ]) result

val decode_bytes: string -> (bytes, [
    `Invalid_base32
  ]) result
