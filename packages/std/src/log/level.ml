open Global

(** Log levels from least to most severe *)
type t =
  Trace
  | Debug
  | Info
  | Warn
  | Error

let to_int =
  function
  | Trace -> 0
  | Debug -> 1
  | Info -> 2
  | Warn -> 3
  | Error -> 4

let to_string =
  function
  | Trace -> "TRACE"
  | Debug -> "DEBUG"
  | Info -> "INFO"
  | Warn -> "WARN"
  | Error -> "ERROR"

let compare = fun l1 l2 ->
  Int.compare (to_int l1) (to_int l2)
