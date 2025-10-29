open Kernel

type timer_resolution = Second | Millisecond | Microsecond | Nanosecond
type t = { timer_resolution : timer_resolution }

let default = { timer_resolution = Millisecond }
let make ?(timer_resolution = Millisecond) () = { timer_resolution }

let resolution_to_nanos = function
  | Second -> 1_000_000_000L
  | Millisecond -> 1_000_000L
  | Microsecond -> 1_000L
  | Nanosecond -> 1L
