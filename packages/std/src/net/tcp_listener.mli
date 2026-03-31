(** TCP listener for accepting connections *)
open Global

type t
(** Create and bind a TCP listener. The socket is automatically set to
    non-blocking mode. *)
type error =
  | Connection_refused
  | Closed
  | System_error of IO.error
val bind : ?reuse_addr:bool ->
?reuse_port:bool ->
?backlog:int ->
Kernel.Net.Addr.stream_addr ->
(t, error) result

(** Accept a connection. This will suspend the process until a connection is
    available. Optionally specify a timeout for the syscall. *)
val accept : ?timeout:Time.Duration.t ->
t ->
(Kernel.Net.Tcp_stream.t * Kernel.Net.Addr.stream_addr, error) result

(** Get the local address the listener is bound to *)
val local_addr : t -> Kernel.Net.Addr.stream_addr

(** Close the listener *)
val close : t -> unit
