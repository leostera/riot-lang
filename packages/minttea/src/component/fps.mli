type t
type tick_result =
  | Frame
  | Skip

val make: float -> t

val from_int: int -> t

val from_float: float -> t

val tick: ?now:Std.Time.Instant.t -> t -> tick_result
