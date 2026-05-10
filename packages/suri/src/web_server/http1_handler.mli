(**
   # HTTP/1.1 Protocol Handler

   Implements HTTP/1.1 request parsing, response sending, and connection
   management with keep-alive support.

   ## Features

   - HTTP/1.1 request parsing with configurable limits
   - Request body and WebSocket frame limits from `Config.t`
   - Keep-alive connection management

   ## Example

   ```ocaml
   let handler _conn req =
     let body = Request.body req in
     Response.ok ~body:"Hello, World!" ()

   let config = Config.make () in
   let state = Http1.make_handler ~config ~handler () in
   state
   ```
*)

type state
type header_name_error =
  | EmptyHeaderName
  | InvalidHeaderNameChar of { char: char; index: int }
type header_value_error =
  | InvalidHeaderValueChar of { char: char; index: int }
type serialization_error =
  | InvalidHeaderName of {
      name: string;
      reason: header_name_error;
    }
  | InvalidHeaderValue of {
      name: string;
      value: string;
      reason: header_value_error;
    }
type io_error =
  | ResponseSerializationFailed of serialization_error
  | ConnectionFailed of Socket_pool.Connection.error
type parse_error =
  | UpstreamParseError of Http.Http1.Common.error
type websocket_key_error =
  | InvalidBase64
  | InvalidLength of { actual: int; expected: int }
type websocket_upgrade_error =
  | InvalidWebSocketMethod of Std.Net.Http.Method.t
  | InvalidWebSocketVersion of Std.Net.Http.Version.t
  | MissingWebSocketUpgrade
  | InvalidWebSocketUpgrade of { value: string }
  | MissingWebSocketConnectionUpgrade
  | MissingWebSocketVersion
  | UnsupportedWebSocketVersion of { value: string; expected: string }
  | MissingWebSocketKey
  | InvalidWebSocketKey of {
      value: string;
      reason: websocket_key_error;
    }
type websocket_frame_limit_error =
  | WebSocketFrameTooLarge of { size: int; limit: int }
  | WebSocketMessageTooLarge of { size: int; limit: int }
type content_length_error =
  | InvalidInteger
  | NegativeLength of int
type request_body_header_error =
  | InvalidContentLength of {
      value: string;
      reason: content_length_error;
    }
  | ConflictingContentLength of {
      values: string list;
    }
  | ContentLengthExceedsLimit of { length: int; limit: int }
  | TransferEncodingWithContentLength of {
      transfer_encoding: string;
      content_lengths: string list;
    }
  | UnsupportedTransferEncoding of { value: string }
type request_header_error =
  | MissingHostHeader
type error =
  | ParseError of parse_error
  | ExcessBodyRead
  | IoError of io_error

val to_string_error: error -> string

(** Wrap an upstream HTTP/1 parser error as a Suri parse error. *)
val parse_error_from_upstream_error: Http.Http1.Common.error -> parse_error

val serialize_response: Response.t -> (string, serialization_error) Std.result

val compute_websocket_accept: string -> string

val validate_websocket_upgrade: Request.t -> (string, websocket_upgrade_error) Std.result

val websocket_upgrade_error_to_string: websocket_upgrade_error -> string

val validate_websocket_frame_limits:
  max_frame_size:int ->
  max_message_size:int ->
  Http.Ws.Frame.t ->
  (unit, websocket_frame_limit_error) Std.result

val websocket_frame_limit_error_to_string: websocket_frame_limit_error -> string

val validate_request_body_headers:
  ?max_body_size:int ->
  Std.Net.Http.Request.t ->
  (int, request_body_header_error) Std.result

val request_body_header_error_to_string: request_body_header_error -> string

val split_request_body: string -> int -> string * string

val validate_request_headers: Std.Net.Http.Request.t -> (unit, request_header_error) Std.result

val request_header_error_to_string: request_header_error -> string

val should_keep_alive: Request.t -> bool

val should_continue_keep_alive:
  max_keep_alive_requests:int ->
  requests_processed:int ->
  Request.t ->
  bool

(** Create a handler that supports WebSocket upgrades via `Http_handler.response`. *)
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
