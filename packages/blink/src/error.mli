open Std

(** Blink client errors. *)
type protocol_error =
  | RequestBudgetExhausted
  | InvalidRequestUri of string
  | UnsupportedWebSocketScheme of string
  | EmptyChunkSize
  | InvalidChunkSize
  | ChunkSizeOverflow
  | InvalidChunkDataLineEnding
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

(** Lift a network error into a Blink error. *)
val from_net_error: Net.error -> t

(** Lift a std I/O error into a Blink error. *)
val from_io_error: IO.error -> t

(** Lift a TLS stream error into a Blink error. *)
val from_tls_error: Net.TlsStream.error -> t

(** Render a Blink protocol error for diagnostics. *)
val protocol_error_to_string: protocol_error -> string

(** Render a Blink websocket handshake error for diagnostics. *)
val handshake_error_to_string: handshake_error -> string

(** Render a Blink transport/protocol error for diagnostics. *)
val to_string: t -> string
