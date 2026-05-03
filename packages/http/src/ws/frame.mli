(** WebSocket Frame Implementation *)
open Std

type opcode =
  | Continuation
  | Text
  | Binary
  | Close
  | Ping
  | Pong
type t = {
  fin: bool;
  rsv1: bool;
  rsv2: bool;
  rsv3: bool;
  opcode: opcode;
  masked: bool;
  payload: string;
}
type close_payload_error =
  | ClosePayloadTooShort of { payload_length: int }
  | InvalidCloseCode of { code: int }
  | InvalidCloseReasonUtf8 of { reason_length: int }

val opcode_to_int: opcode -> int

val opcode_of_int: int -> opcode option

val unmask: int32 -> string -> string

val generate_mask: ?rng:Random.Rng.t -> unit -> (int32, Random.error) result

val apply_mask: int32 -> string -> string

val close_payload_error_to_string: close_payload_error -> string

val validate_close_payload: string -> (unit, close_payload_error) result

(* Frame constructors *)
val text: ?fin:bool -> string -> t

val binary: ?fin:bool -> string -> t

val close: ?payload:string -> unit -> t

val ping: ?payload:string -> unit -> t

val pong: ?payload:string -> unit -> t

val continuation: ?fin:bool -> string -> t
