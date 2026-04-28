(**
   Stateful WebSocket message assembly.

   The frame parser validates individual wire frames. This module validates
   message-level sequencing across frames, especially fragmentation.
*)
open Std

type t
type data_opcode =
  | Text
  | Binary
type control_opcode =
  | Close
  | Ping
  | Pong
type event =
  | DataMessage of { opcode: data_opcode; payload: string }
  | ControlFrame of Frame.t
type error =
  | InvalidMessageSizeLimit of { max_message_size: int }
  | ContinuationWithoutFragment
  | DataFrameWhileFragmented of { opcode: data_opcode }
  | FragmentedControlFrame of { opcode: control_opcode }
  | ControlFramePayloadTooLarge of { opcode: control_opcode; payload_length: int }
  | InvalidClosePayload of Frame.close_payload_error
  | MessagePayloadTooLarge of { payload_length: int; max_message_size: int }
  | InvalidTextMessageUtf8 of { payload_length: int }
val error_to_string: error -> string

val create: ?max_message_size:int -> unit -> (t, error) result

val handle_frame: t -> Frame.t -> (t * event option, error) result
