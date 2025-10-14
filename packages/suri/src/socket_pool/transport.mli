(** Transport layer abstraction.

    Abstracts over different transport protocols (TCP, TLS, etc.) to allow
    pluggable connection handling. *)

type t
(** Transport layer type *)

val tcp : unit -> t
(** [tcp ()] creates a plain TCP transport *)

val handshake :
  t ->
  accepted_at:Std.Time.Instant.t ->
  stream:Std.Net.TcpStream.t ->
  peer:Std.Net.Addr.stream_addr ->
  buffer_size:int ->
  (Connection.t, [> `Closed ]) result
(** [handshake transport ~accepted_at ~stream ~peer ~buffer_size] performs
    transport-specific handshake and returns a connection.

    For TCP this is a no-op, but for TLS this would perform the TLS handshake.
*)
