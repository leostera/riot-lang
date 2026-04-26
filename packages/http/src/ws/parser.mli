(** WebSocket Frame Parser *)
open Std

(** Parse a WebSocket frame incrementally from string *)
type 'a parse_result =
  | Done of { value: 'a; remaining: string }
  | Need_more
  | Error of string
val parse: string -> Frame.t parse_result
