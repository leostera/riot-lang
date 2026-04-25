(** Ascii85/Base85 encoding and decoding. *)
open Global

val encode: string -> string

val encode_bytes: bytes -> string

val decode: string -> (string, [`Invalid_base85]) result

val decode_bytes: string -> (bytes, [`Invalid_base85]) result
