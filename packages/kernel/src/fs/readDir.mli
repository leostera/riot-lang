open Async

type t

val open_ : string -> (t, IO.error) result
val read : t -> (string, IO.error) result
val close : t -> (unit, IO.error) result
