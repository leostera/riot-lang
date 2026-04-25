(** Hexadecimal encoding and decoding. *)
open Global

val encode: string -> string

val encode_lower: string -> string

val encode_bytes: bytes -> string

val encode_bytes_lower: bytes -> string

val decode: string -> (string, [`Invalid_base16]) result

val decode_bytes: string -> (bytes, [`Invalid_base16]) result
