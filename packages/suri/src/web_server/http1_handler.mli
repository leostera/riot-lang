(**
   # HTTP/1.1 Protocol Handler

   Implements HTTP/1.1 request parsing, response sending, and connection
   management with keep-alive support.

   ## Features

   - HTTP/1.1 request parsing with configurable limits
   - Keep-alive connection management

   ## Example

   ```ocaml let handler _conn req = let body = Request.body req in Response.ok
   ~body:"Hello, World!" ()

   let config = Config.make () in let state = Http1.make_handler ~config
   ~handler () in (* Use with Socket_pool *) ```
*)

type state
type error = [ | `ParseError of string | `ExcessBodyRead | `IoError of string]
val to_string_error: error -> string

module For_testing: sig
  type serialization_error =
    | InvalidHeaderName of string
    | InvalidHeaderValue of { name: string; value: string }
  type websocket_upgrade_error =
    | InvalidWebSocketMethod of Std.Net.Http.Method.t
    | InvalidWebSocketVersion of Std.Net.Http.Version.t
    | MissingWebSocketUpgrade
    | InvalidWebSocketUpgrade of string
    | MissingWebSocketConnectionUpgrade
    | MissingWebSocketVersion
    | UnsupportedWebSocketVersion of string
    | MissingWebSocketKey
    | InvalidWebSocketKey of string
  type request_body_header_error =
    | InvalidContentLength of string
    | ConflictingContentLength of string list
    | TransferEncodingWithContentLength
    | UnsupportedTransferEncoding of string
  type request_header_error =
    | MissingHostHeader
  val serialize_response: Response.t -> (string, serialization_error) Std.result

  val compute_websocket_accept: string -> string

  val validate_websocket_upgrade: Request.t -> (string, websocket_upgrade_error) Std.result

  val websocket_upgrade_error_to_string: websocket_upgrade_error -> string

  val validate_request_body_headers:
    Std.Net.Http.Request.t ->
    (int, request_body_header_error) Std.result

  val request_body_header_error_to_string: request_body_header_error -> string

  val split_request_body: string -> int -> string * string

  val validate_request_headers: Std.Net.Http.Request.t -> (unit, request_header_error) Std.result

  val request_header_error_to_string: request_header_error -> string
end

(** Create a handler that supports WebSocket upgrades via {!Http_handler.response}. *)
val make_handler:
  config:Super.Config.t ->
  handler:Http_handler.t ->
  ?sniffed_data:string ->
  unit ->
  state

(** Handler functions for Socket_pool integration *)
val handle_close: Socket_pool.Connection.t -> state -> unit

val handle_connection:
  Socket_pool.Connection.t ->
  state ->
  (state, error) Socket_pool.Handler.handler_result

val handle_data:
  string ->
  Socket_pool.Connection.t ->
  state ->
  (state, error) Socket_pool.Handler.handler_result

val handle_error:
  error ->
  Socket_pool.Connection.t ->
  state ->
  (state, error) Socket_pool.Handler.handler_result

val handle_shutdown:
  Socket_pool.Connection.t ->
  state ->
  (state, error) Socket_pool.Handler.handler_result

val handle_message:
  Std.Message.t ->
  Socket_pool.Connection.t ->
  state ->
  (state, error) Socket_pool.Handler.handler_result
