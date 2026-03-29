open Global

val encode : string -> string

val encode_bytes : bytes -> string

val decode : string -> (string, [
  | `Invalid_base64
]) result
