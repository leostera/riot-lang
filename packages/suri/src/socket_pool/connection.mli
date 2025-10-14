(** Active TCP connection management.

    Represents a single active connection to a client with operations for
    sending, receiving, and querying connection metadata. *)

type t
(** An active TCP connection with GADT constructor for protocol negotiation *)

val make :
  ?protocol:string option ->
  accepted_at:Std.Time.Instant.t ->
  stream:Std.Net.TcpStream.t ->
  buffer_size:int ->
  peer:Std.Net.Addr.stream_addr ->
  unit ->
  t
(** [make ~accepted_at ~stream ~buffer_size ~peer ()] creates a new connection.

    [protocol] is the negotiated protocol (e.g., "h2" for HTTP/2) *)

val negotiated_protocol : t -> string option
(** [negotiated_protocol conn] returns the negotiated protocol if any *)

val send : t -> string -> (unit, [> `Closed ]) result
(** [send conn data] sends [data] through the connection.

    Returns [Ok ()] on success or [Error `Closed] if connection closed. *)

val receive : ?limit:int -> ?read_size:int -> t -> (string, [> `Closed ]) result
(** [receive ?limit ?read_size conn] reads data from the connection.

    - [limit] sets the maximum bytes to read (default 1024)
    - [read_size] overrides the default buffer read size

    Returns [Ok data] with received data, or [Error `Closed] if closed. *)

val peer : t -> Std.Net.Addr.stream_addr
(** [peer conn] returns the remote peer's address *)

val connected_at : t -> Std.Time.Instant.t
(** [connected_at conn] returns when this connection was established *)

val accepted_at : t -> Std.Time.Instant.t
(** [accepted_at conn] returns when this connection was accepted *)

val close : t -> unit
(** [close conn] closes the connection and logs duration *)

val send_file :
  t -> ?off:int -> len:int -> string -> (unit, [> `Closed ]) result
(** [send_file conn ?off ~len path] sends a file through the connection.

    TODO: Not yet implemented - will use sendfile optimization when available *)
