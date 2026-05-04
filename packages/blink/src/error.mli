open Std

(** Blink client errors. *)
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

(** Lift a network error into a Blink error. *)
val from_net_error: Net.error -> t

(** Lift a std I/O error into a Blink error. *)
val from_io_error: IO.error -> t

(** Lift a TLS stream error into a Blink error. *)
val from_tls_error: Net.TlsStream.error -> t

(** Render a Blink transport/protocol error for diagnostics. *)
val to_string: t -> string
