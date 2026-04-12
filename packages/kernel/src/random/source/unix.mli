type error =
  | System of System_error.t
val error_to_string: error -> string

val fill_bytes: bytes -> (unit, error) Result.t
