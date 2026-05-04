open Std

type t =
  | NetError of Net.error
  | TlsError of Net.TlsStream.error
  | ParseError of Http.Http1.Common.error
  | WebSocketParseError of Http.Ws.Parser.error
  | WebSocketSerializeError of Http.Ws.Serializer.error
  | ProtocolError of string
  | HandshakeFailed of string
  | InvalidFrame
  | Eof
  | Closed

let from_net_error = fun e -> NetError e

let from_io_error = fun __tmp1 ->
  match __tmp1 with
  | IO.Connection_refused -> NetError Net.Connection_refused
  | IO.Closed -> NetError Net.Closed
  | error -> NetError (Net.System_error error)

let from_tls_error = fun e -> TlsError e

let to_string = fun value ->
  match value with
  | NetError Net.Connection_refused -> "connection refused"
  | NetError Net.Closed -> "connection closed"
  | NetError (Net.System_error error) -> "network system error: " ^ IO.error_message error
  | TlsError Net.TlsStream.Closed -> "tls closed"
  | TlsError (Net.TlsStream.Handshake_failed message) -> "tls handshake failed: " ^ message
  | TlsError (Net.TlsStream.System_error error) -> "tls system error: " ^ IO.error_message error
  | TlsError (Net.TlsStream.Network_read_failed _) -> "tls network read failed"
  | TlsError (Net.TlsStream.Network_write_failed _) -> "tls network write failed"
  | TlsError Net.TlsStream.Tls_not_available -> "tls not available"
  | TlsError Net.TlsStream.Unsupported_vectored_operation -> "unsupported tls vectored operation"
  | ParseError error -> "parse error: " ^ Http.Http1.Common.error_to_string error
  | WebSocketParseError error -> "websocket parse error: " ^ Http.Ws.Parser.error_to_string error
  | WebSocketSerializeError error ->
      "websocket serialize error: " ^ Http.Ws.Serializer.error_to_string error
  | ProtocolError message -> "protocol error: " ^ message
  | HandshakeFailed message -> "handshake failed: " ^ message
  | InvalidFrame -> "invalid frame"
  | Eof -> "eof"
  | Closed -> "closed"
