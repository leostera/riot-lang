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
val opcode_to_int: opcode -> int

val opcode_of_int: int -> opcode option

val unmask: int32 -> string -> string

val generate_mask: unit -> int32

val apply_mask: int32 -> string -> string

(* Frame constructors *)

val text: ?fin:bool -> string -> t

val binary: ?fin:bool -> string -> t

val close: ?payload:string -> unit -> t

val ping: ?payload:string -> unit -> t

val pong: ?payload:string -> unit -> t

val continuation: ?fin:bool -> string -> t
