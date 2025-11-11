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

let of_net_error e = Net_error e
let of_tls_error e = Tls_error e
