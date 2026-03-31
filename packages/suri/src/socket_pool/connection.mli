open Std

(** Active TCP connection management.

    Represents a single active connection to a client with operations for
    sending, receiving, and querying connection metadata. *)
(** An active TCP connection with GADT constructor for protocol negotiation *)
(** [make ~accepted_at ~stream ~buffer_size ~peer ()] creates a new connection.

    [protocol] is the negotiated protocol (e.g., "h2" for HTTP/2) *)
type t
val make : ?protocol:string option ->
accepted_at:Std.Time.Instant.t ->
stream:Std.Net.TcpStream.t ->
buffer_size:int ->
peer:Std.Net.Addr.stream_addr ->
unit ->
t

(** [negotiated_protocol conn] returns the negotiated protocol if any *)
val negotiated_protocol : t -> string option

(** [send conn data] sends [data] through the connection.

    Returns [Ok ()] on success or [Error `Closed] if connection closed. *)
val send : t -> string -> (unit, [>
  | `Closed
]) result

(** [receive ?limit ?read_size ?timeout conn] reads data from the connection.

    - [limit] sets the maximum bytes to read (default 1024)
    - [read_size] overrides the default buffer read size
    - [timeout] optional timeout duration for the read operation

    Returns [Ok data] with received data, or [Error `Closed] if closed.
    Raises [Syscall_timeout] if timeout is specified and expires. *)
val receive : ?limit:int -> ?read_size:int -> ?timeout:Std.Time.Duration.t -> t -> (string, [>
  | `Closed
]) result

(** [peer conn] returns the remote peer's address *)
val peer : t -> Std.Net.Addr.stream_addr

(** [connected_at conn] returns when this connection was established *)
val connected_at : t -> Std.Time.Instant.t

(** [accepted_at conn] returns when this connection was accepted *)
val accepted_at : t -> Std.Time.Instant.t

(** [close conn] closes the connection and logs duration *)
val close : t -> unit

(** [stream conn] returns the underlying TCP stream *)
val stream : t -> Std.Net.TcpStream.t

(** [send_file conn ?off ~len path] sends a file through the connection.

    TODO: Not yet implemented - will use sendfile optimization when available *)
val send_file : t -> ?off:int -> len:int -> string -> (unit, [>
  | `Closed
]) result
