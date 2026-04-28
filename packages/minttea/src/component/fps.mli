type t
type tick_result =
  | Frame
  | Skip
val make: float -> t

val of_int: int -> t

val of_float: float -> t

val tick: ?now:Std.Time.Instant.t -> t -> tick_result
