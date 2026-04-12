type error =
  | System of System_error.t
val error_to_string: error -> string

(** Use `fill_bytes bytes` to overwrite `bytes` with platform entropy. *)
val fill_bytes: bytes -> (unit, error) Result.t
