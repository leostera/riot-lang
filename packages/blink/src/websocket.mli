open Std

type t
type error = Error.t
type message =
  | Text of string
  | Binary of string
  | Ping of string
  | Pong of string
  | Close of int option * string
val connect: Net.Uri.t -> (t, error) result

val send_text: t -> string -> (unit, error) result

val send_binary: t -> string -> (unit, error) result

val send_ping: t -> ?payload:string -> unit -> (unit, error) result

val send_pong: t -> ?payload:string -> unit -> (unit, error) result

val send_close: t -> ?code:int -> ?reason:string -> unit -> (unit, error) result

val receive: t -> (message, error) result

val close: t -> unit
