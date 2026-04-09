type t
type error =
  | InvalidTimeoutNs of { timeout_ns: int64 }
val error_to_string: error -> string

val after_ns: int64 -> (t, error) Result.t

val every_ns: int64 -> (t, error) Result.t

val timeout_ns: t -> int64

val repeats: t -> bool

val to_source: t -> Async.Source.t
