(** Normalized request values accepted by the managed client. *)
module Request: module type of Request

(** Normalized response values returned by the managed client. *)
module Response: module type of Response

(** Token-bucket request budgets. *)
module Budget: module type of Budget

(** Request telemetry emitted after each terminal result. *)
module Telemetry: module type of Telemetry

(** Managed client configuration. *)
module Config: module type of Config

type transport = Config.transport
type error = {
  class_: Response.error_class;
  message: string;
  telemetry: Telemetry.t;
}
type t
type connection
type message = Connection.message

val make: ?config:Config.t -> unit -> t

val execute: t -> Request.t -> (Response.t * Telemetry.t, error) Std.result

val connect: t -> Std.Net.Uri.t -> (connection, Error.t) Std.result

val request:
  t ->
  connection ->
  Std.Net.Http.Request.t ->
  ?body:string ->
  unit ->
  (unit, Error.t) Std.result

val stream: t -> connection -> (message list, Error.t) Std.result

val messages:
  ?on_message:(message list -> unit) ->
  t ->
  connection ->
  (message list, Error.t) Std.result

val await:
  ?on_message:(message list -> unit) ->
  t ->
  connection ->
  (Std.Net.Http.Response.t * string, Error.t) Std.result

val close: t -> connection -> unit

val error_to_string: error -> string

val budget_remaining: t -> int

module SSE: sig
  type event = Sse.event = {
    data: string;
    event_type: string option;
    id: string option;
  }

  val await: t -> connection -> event Std.Iter.MutIterator.t
end

module WebSocket: sig
  type client = t
  type t = Websocket.t
  type error = Error.t
  type message = Websocket.message =
    | Text of string
    | Binary of string
    | Ping of string
    | Pong of string
    | Close of int option * string

  val connect: client -> Std.Net.Uri.t -> (t, error) Std.result

  val send_text: client -> t -> string -> (unit, error) Std.result

  val send_binary: client -> t -> string -> (unit, error) Std.result

  val send_ping: client -> t -> ?payload:string -> unit -> (unit, error) Std.result

  val send_pong: client -> t -> ?payload:string -> unit -> (unit, error) Std.result

  val send_close: client -> t -> ?code:int -> ?reason:string -> unit -> (unit, error) Std.result

  val receive: client -> t -> (message, error) Std.result

  val close: client -> t -> unit
end

val shutdown: t -> unit
