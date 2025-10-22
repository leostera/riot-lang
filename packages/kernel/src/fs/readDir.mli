open Async

type t

val open_ : string -> (t, [> io_error ]) io_result
val read : t -> (string, [> io_error ]) io_result
val close : t -> (unit, [> io_error ]) io_result
