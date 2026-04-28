(** WebSocket Frame Parser *)
open Std

(** Side of the connection parsing an incoming WebSocket frame. *)
type role =
  | Server
  | Client
(** Parse a WebSocket frame incrementally from string *)
type 'a parse_result =
  | Done of { value: 'a; remaining: string }
  | Need_more
  | Error of error

and error =
  | InvalidOpcode of int
  | ReservedBitsSet
  | ClientFrameNotMasked
  | ServerFrameMasked
  | FragmentedControlFrame
  | ControlFramePayloadTooLarge of { payload_length: int }
val error_to_string: error -> string

val parse: role:role -> string -> Frame.t parse_result
