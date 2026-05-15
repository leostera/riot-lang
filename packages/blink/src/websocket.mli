open Std

(** WebSocket connection handle. *)
type t
(** WebSocket operation error. *)
type error = Error.t
(** WebSocket message. *)
type message =
  | Text of string
  | Binary of string
  | Ping of string
  | Pong of string
  | Close of int option * string

(** Connect to a WebSocket endpoint. *)
val connect: Net.Uri.t -> (t, error) result

(** Send a text frame. *)
val send_text: t -> string -> (unit, error) result

(** Send a binary frame. *)
val send_binary: t -> string -> (unit, error) result

(** Send a ping frame. *)
val send_ping: t -> ?payload:string -> unit -> (unit, error) result

(** Send a pong frame. *)
val send_pong: t -> ?payload:string -> unit -> (unit, error) result

(** Send a close frame. *)
val send_close: t -> ?code:int -> ?reason:string -> unit -> (unit, error) result

(** Receive the next WebSocket message. *)
val receive: t -> (message, error) result

(** Close the WebSocket connection handle. *)
val close: t -> (unit, error) result
