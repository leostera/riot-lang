open Std

(** Blink client errors. *)
type t =
  | NetError of Net.error
  | TlsError of Net.TlsStream.error
  | ParseError of string
  | ProtocolError of string
  | HandshakeFailed of string
  | InvalidFrame
  | Eof
  | Closed

(** Lift a network error into a Blink error. *)
val of_net_error: Net.error -> t

(** Lift a std I/O error into a Blink error. *)
val of_io_error: IO.error -> t

(** Lift a TLS stream error into a Blink error. *)
val of_tls_error: Net.TlsStream.error -> t
