open Std

type t =
  | Net_error of Net.error
  | Tls_error of Net.TlsStream.error
  | Parse_error of string
  | Protocol_error of string
  | Handshake_failed of string
  | Invalid_frame
  | Eof
  | Closed
val of_net_error : Net.error -> t

val of_tls_error : Net.TlsStream.error -> t
