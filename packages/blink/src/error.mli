open Std

type t =
  | NetError of Net.error
  | TlsError of Net.TlsStream.error
  | ParseError of string
  | ProtocolError of string
  | HandshakeFailed of string
  | InvalidFrame
  | Eof
  | Closed
val of_net_error: Net.error -> t

val of_tls_error: Net.TlsStream.error -> t
