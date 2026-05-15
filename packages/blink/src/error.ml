open Std

type protocol_error =
  | RequestBudgetExhausted
  | InvalidRequestUri of string
  | UnsupportedWebSocketScheme of string
  | EmptyChunkSize
  | InvalidChunkSize
  | ChunkSizeOverflow
  | InvalidChunkDataLineEnding
  | IncompleteSseEvent
  | TransportRaised of string
  | ApplicationTransportError of string

type handshake_error =
  | ConnectionClosedDuringHandshake
  | SwitchingProtocolsExpected
  | InvalidAcceptHeader

type t =
  | NetError of Net.error
  | TlsError of Net.TlsStream.error
  | ParseError of Http.Http1.Common.error
  | WebSocketParseError of Http.Ws.Parser.error
  | WebSocketSerializeError of Http.Ws.Serializer.error
  | ProtocolError of protocol_error
  | HandshakeFailed of handshake_error
  | RequestFailed of t
  | ResponseFailed of t
  | InvalidFrame
  | Eof
  | Closed

let from_net_error = fun e -> NetError e

let from_io_error = fun error ->
  match error with
  | IO.Connection_refused -> NetError Net.Connection_refused
  | IO.Closed -> NetError Net.Closed
  | error -> NetError (Net.System_error error)

let from_tls_error = fun e -> TlsError e

let protocol_error_to_string = fun error ->
  match error with
  | RequestBudgetExhausted -> "request budget exhausted"
  | InvalidRequestUri uri -> "invalid request uri: " ^ uri
  | UnsupportedWebSocketScheme scheme -> "unsupported websocket scheme: " ^ scheme
  | EmptyChunkSize -> "empty chunk size"
  | InvalidChunkSize -> "invalid chunk size"
  | ChunkSizeOverflow -> "chunk size overflow"
  | InvalidChunkDataLineEnding -> "invalid chunk data line ending"
  | IncompleteSseEvent -> "incomplete server-sent event"
  | TransportRaised reason -> "transport raised: " ^ reason
  | ApplicationTransportError reason -> "application transport error: " ^ reason

let handshake_error_to_string = fun error ->
  match error with
  | ConnectionClosedDuringHandshake -> "connection closed during handshake"
  | SwitchingProtocolsExpected -> "server did not return 101 Switching Protocols"
  | InvalidAcceptHeader -> "invalid Sec-WebSocket-Accept header"

let rec to_string = fun value ->
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
  | ProtocolError error -> "protocol error: " ^ protocol_error_to_string error
  | HandshakeFailed error -> "handshake failed: " ^ handshake_error_to_string error
  | RequestFailed error -> "request failed: " ^ to_string error
  | ResponseFailed error -> "response failed: " ^ to_string error
  | InvalidFrame -> "invalid frame"
  | Eof -> "eof"
  | Closed -> "closed"
