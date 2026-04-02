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

let of_net_error = fun e -> NetError e

let of_tls_error = fun e -> TlsError e
