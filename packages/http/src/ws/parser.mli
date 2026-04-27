(** WebSocket Frame Parser *)
open Std

(** Parse a WebSocket frame incrementally from string *)
type 'a parse_result =
  | Done of { value: 'a; remaining: string }
  | Need_more
  | Error of error

and error =
  | InvalidOpcode of int
  | ReservedBitsSet
  | FragmentedControlFrame
  | ControlFramePayloadTooLarge of { payload_length: int }
val error_to_string: error -> string

val parse: string -> Frame.t parse_result
