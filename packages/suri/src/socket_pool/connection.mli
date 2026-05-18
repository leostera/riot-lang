open Std

(**
   Active TCP connection management.

   Represents a single active connection to a client with operations for
   sending, receiving, and querying connection metadata.
*)

(** An active TCP connection with GADT constructor for protocol negotiation *)

(**
   [make ~accepted_at ~stream ~buffer_size ~peer ()] creates a new connection.

   [protocol] is the negotiated protocol (e.g., "h2" for HTTP/2)
*)
type t
type send_file_range_error = { off: int; len: int; size: int }
type error =
  | Closed
  | ReadError of Std.Net.TcpStream.error
  | WriteError of Std.Net.TcpStream.error
  | FileError of Std.Fs.error
  | InvalidRange of send_file_range_error
type send_file_error = error

val error_to_string: error -> string

val make:
  ?protocol:string option ->
  accepted_at:Std.Time.Instant.t ->
  stream:Std.Net.TcpStream.t ->
  buffer_size:int ->
  peer:Std.Net.Addr.stream_addr ->
  unit ->
  t

(** [negotiated_protocol conn] returns the negotiated protocol if any *)
val negotiated_protocol: t -> string option

(**
   [send conn data] sends [data] through the connection.

   Returns [Ok ()] on success, [Error Closed] for a zero-byte write, or
   [Error (WriteError _)] when the stream write fails.
*)
val send: t -> string -> (unit, error) result

(**
   [receive ?limit ?read_size ?timeout conn] reads data from the connection.

   - [limit] sets the maximum bytes to read (default: configured read size)
   - [read_size] overrides the default buffer read size
   - [timeout] optional timeout duration for the read operation

   Returns [Ok data] with received data, [Error Closed] if closed, or
   [Error (ReadError _)] when the stream read fails.
   Raises [Syscall_timeout] if timeout is specified and expires.
*)
val receive:
  ?limit:int ->
  ?read_size:int ->
  ?timeout:Std.Time.Duration.t ->
  t ->
  (string, error) result

(** [peer conn] returns the remote peer's address *)
val peer: t -> Std.Net.Addr.stream_addr

(** [connected_at conn] returns when this connection was established *)
val connected_at: t -> Std.Time.Instant.t

(** [accepted_at conn] returns when this connection was accepted *)
val accepted_at: t -> Std.Time.Instant.t

(** [close conn] closes the connection and logs duration *)
val close: t -> unit

(** [stream conn] returns the underlying TCP stream *)
val stream: t -> Std.Net.TcpStream.t

(**
   [send_file conn ?off ~len path] sends a file through the connection.

   The current implementation reads the requested file, validates the requested
   slice, and writes all bytes to the socket. It returns [FileError] when the
   file cannot be read, [InvalidRange] when [off] or [len] are outside the file
   bounds, [Closed] when the socket cannot be written completely, and
   [WriteError _] when the stream write fails.
*)
val send_file: t -> ?off:int -> len:int -> string -> (unit, send_file_error) result

(** [write_all_with ~write data] writes all bytes in [data] through [write]. *)
val write_all_with:
  write:(bytes -> pos:int -> len:int -> (int, Std.Net.TcpStream.error) result) ->
  string ->
  (unit, error) result

(** [send_file_slice ?off ~len content] validates and extracts a file payload slice. *)
val send_file_slice: ?off:int -> len:int -> string -> (string, error) result
