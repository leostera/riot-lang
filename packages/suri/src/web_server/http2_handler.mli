open Std

(**
   # HTTP/2 Protocol Handler (Experimental)

   Prototype HTTP/2 request parsing, response sending, and stream
   multiplexing support. Connection-level limits are provided by {!Config.t}.

   This handler is not wired into {!Web_server.start_link} by default and is
   not yet a production-compliant HTTP/2 implementation.

   ## Features

   - HTTP/2 frame parsing using reentrant parser
   - Stream multiplexing with concurrent request handling
   - HPACK header compression/decompression
   - Server push support (optional)
   - Flow control

   ## Usage

   HTTP/2 connections are typically established via:
   1. Prior knowledge (direct HTTP/2 connection)
   2. HTTP/1.1 Upgrade (not yet implemented)
   3. TLS ALPN negotiation (requires TLS support)

   For now, HTTP/2 is detected by the connection preface:
   `PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n`
*)

type state
type protocol_error =
  | UpgradeNotSupported
  | UnknownDataStream of int
  | InvalidPreface
  | InvalidRequestHeaders of request_header_error

and pseudo_header =
  | Method
  | Scheme
  | Path

and request_header_error =
  | MissingPseudoHeader of pseudo_header
  | EmptyPseudoHeader of pseudo_header
  | InvalidPath of {
      value: string;
      reason: Std.Net.Uri.error;
    }
type io_operation =
  | SendSettings
  | SendSettingsAck
  | SendHeaders
  | SendData
  | SendPing
type error =
  | ParseError of Http.Http2.Parser_reader.parse_error
  | SerializerError of Http.Http2.Serializer.error
  | FrameConstructorError of Http.Http2.Frame.constructor_error
  | HpackEncodeError of Http.Http2.Hpack.encode_error
  | HpackDecodeError of Http.Http2.Hpack.decode_error
  | ProtocolError of protocol_error
  | IoError of {
      operation: io_operation;
      error: Socket_pool.Connection.error;
    }

val to_string_error: error -> string

val pseudo_header_to_string: pseudo_header -> string

val request_header_error_to_string: request_header_error -> string

val headers_to_request:
  Http.Http2.Hpack.header list ->
  string ->
  (Request.t, request_header_error) result

(**
   Create HTTP/2 handler state

   @param config Server configuration
   @param handler Request handler function (receives parsed request)
   @param sniffed_data Optional data already read during protocol detection
   @return Initial handler state
*)
val make_handler:
  config:Super.Config.t ->
  handler:Http_handler.t ->
  ?sniffed_data:string ->
  unit ->
  state

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
