open Global

(** Log levels from least to most severe *)
type t =
  | Trace
  | Debug
  | Info
  | Warn
  | Error

let to_int = fun __tmp1 ->
  match __tmp1 with
  | Trace -> 0
  | Debug -> 1
  | Info -> 2
  | Warn -> 3
  | Error -> 4

let to_string = fun __tmp1 ->
  match __tmp1 with
  | Trace -> "TRACE"
  | Debug -> "DEBUG"
  | Info -> "INFO"
  | Warn -> "WARN"
  | Error -> "ERROR"

let compare = fun l1 l2 -> Int.compare (to_int l1) (to_int l2)
