open Std

(** Protocol descriptors exposed by Blink. *)
module Protocol : module type of Protocol

(** Transport backends exposed by Blink. *)
module Transport : module type of Transport

(** HTTP connection interface exposed by Blink. *)
module Connection : module type of Connection

(** WebSocket support exposed by Blink. *)
module WebSocket : module type of Websocket

(** Blink error definitions. *)
module Error : module type of Error

(** Server-Sent Events support exposed by Blink. *)
module SSE : module type of Sse

(** Configurable HTTP client with budgets, telemetry, and connection pooling. *)
module Client : module type of Client

(** Deterministic HTTP recording and replay helpers for tests. *)
module Testing : module type of Testing

(** Blink client error. *)
type error = Error.t
(** Streaming HTTP message from a connection. *)
type message = Connection.message

(** Connect to an HTTP endpoint. *)
val connect : ?read_timeout:Time.Duration.t -> Net.Uri.t -> (Connection.t, error) result

(** Send an HTTP request on an existing connection. *)
val request : Connection.t -> Net.Http.Request.t -> ?body:string -> unit -> (unit, error) result

(** Stream the next batch of connection messages. *)
val stream : Connection.t -> (message list, error) result

(** Collect messages until the response body completes. *)
val messages : ?on_message:(message list -> unit) -> Connection.t -> (message list, error) result

(** Wait for the response to complete and return the response plus body. *)
val await :
  ?on_message:(message list -> unit) ->
  Connection.t ->
  (Net.Http.Response.t * string, error) result

(** Close the connection. *)
val close : Connection.t -> unit
