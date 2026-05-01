(** HTTP/2 Frame Types and Definitions (RFC 7540) *)
open Std

(** Frame type codes *)
type frame_type =
  | Data
  | Headers
  | Priority
  | RstStream
  | Settings
  | PushPromise
  | Ping
  | Goaway
  | WindowUpdate
  | Continuation
  | Unknown of int
type flags = {
  end_stream: bool;
  (** Bit 0 *)
  end_headers: bool;
  (** Bit 2 *)
  padded: bool;
  (** Bit 3 *)
  priority: bool;
  (** Bit 5 *)
  ack: bool;
  (** Bit 0 for SETTINGS/PING *)
}
(** Frame flags *)
type stream_id = int

(** Stream identifier (31-bit unsigned integer) *)

(** Error codes *)
type error_code =
  | NoError
  | ProtocolError
  | InternalError
  | FlowControlError
  | SettingsTimeout
  | StreamClosed
  | FrameSizeError
  | RefusedStream
  | Cancel
  | CompressionError
  | ConnectError
  | EnhanceYourCalm
  | InadequateSecurity
  | Http11Required
  | UnknownErrorCode of int
(** SETTINGS parameters *)
type setting =
  | HeaderTableSize of int
  | EnablePush of bool
  | MaxConcurrentStreams of int
  | InitialWindowSize of int
  | MaxFrameSize of int
  | MaxHeaderListSize of int
(** Frame payload types *)
type payload =
  | DataPayload of {
      data: string;
      pad_length: int option;
    }
  | HeadersPayload of {
      pad_length: int option;
      stream_dependency: int option;
      (** Present if PRIORITY flag set *)
      weight: int option;
      exclusive: bool;
      header_block_fragment: string;
    }
  | PriorityPayload of { stream_dependency: int; exclusive: bool; weight: int }
  | RstStreamPayload of error_code
  | SettingsPayload of setting list
  | PushPromisePayload of {
      pad_length: int option;
      promised_stream_id: int;
      header_block_fragment: string;
    }
  | PingPayload of string
  (** 8 bytes *)
  | GoawayPayload of {
      last_stream_id: int;
      error_code: error_code;
      debug_data: string;
    }
  | WindowUpdatePayload of int
  (** Window size increment *)
  | ContinuationPayload of string
  | UnknownPayload of string

(** Header block fragment *)

(** HTTP/2 Frame *)
type t = {
  length: int;
  (** Payload length (24-bit) *)
  frame_type: frame_type;
  flags: flags;
  stream_id: stream_id;
  payload: payload;
}
(** HTTP/2 frame construction errors. *)
type constructor_error =
  | InvalidPingPayloadLength of { length: int }
  | InvalidWindowUpdateIncrement of { increment: int }

(** Render a frame construction error for diagnostics. *)
val constructor_error_to_string: constructor_error -> string

val default_flags: flags

(** Default flags (all false) *)
val data: stream_id:stream_id -> ?end_stream:bool -> ?pad_length:int -> string -> t

(** Create specific frame types *)
val headers:
  stream_id:stream_id ->
  ?end_stream:bool ->
  ?end_headers:bool ->
  ?pad_length:int ->
  ?priority:int * bool * int ->
  string ->
  t

val priority: stream_id:stream_id -> stream_dependency:int -> exclusive:bool -> weight:int -> t

val rst_stream: stream_id:stream_id -> error_code -> t

val settings: ?ack:bool -> setting list -> t

val push_promise: stream_id:stream_id -> promised_stream_id:int -> ?pad_length:int -> string -> t

(** Create a PING frame. The opaque data must be exactly 8 bytes. *)
val ping: ?ack:bool -> string -> (t, constructor_error) Result.t

val goaway: last_stream_id:int -> error_code:error_code -> ?debug_data:string -> unit -> t

(** Create a WINDOW_UPDATE frame. The increment must be in 1..2^31-1. *)
val window_update: stream_id:stream_id -> int -> (t, constructor_error) Result.t

val continuation: stream_id:stream_id -> ?end_headers:bool -> string -> t

val error_code_to_int: error_code -> int

(** Convert error code to integer *)
val int_to_error_code: int -> error_code option

(** Convert integer to error code *)
