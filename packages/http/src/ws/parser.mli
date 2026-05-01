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
  | PayloadLengthHighBitSet of { first_byte: int }
  | PayloadLengthTooLarge of { most_significant_byte: int; max_payload_length: int }
  | PayloadLengthExceedsLimit of { payload_length: int; max_payload_length: int }
  | InvalidPayloadLengthLimit of { max_payload_length: int }
  | NonMinimalPayloadLength of {
      encoding: payload_length_encoding;
      payload_length: int;
    }
  | InvalidTextPayloadUtf8 of { payload_length: int }
  | ClosePayloadTooShort of { payload_length: int }
  | InvalidCloseCode of { code: int }
  | InvalidCloseReasonUtf8 of { reason_length: int }

and payload_length_encoding =
  | PayloadLength16
  | PayloadLength64

val error_to_string: error -> string

val parse: ?max_payload_length:int -> role:role -> string -> Frame.t parse_result
