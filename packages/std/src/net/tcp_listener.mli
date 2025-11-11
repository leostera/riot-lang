(** TCP listener for accepting connections *)

open Global

type t

type error =
  | Connection_refused
  | Closed
  | System_error of string

val bind :
  ?reuse_addr:bool ->
  ?reuse_port:bool ->
  ?backlog:int ->
  Kernel.Net.Addr.stream_addr ->
  (t, error) result
(** Create and bind a TCP listener. The socket is automatically set to
    non-blocking mode. *)

val accept :
  t -> (Kernel.Net.Tcp_stream.t * Kernel.Net.Addr.stream_addr, error) result
(** Accept a connection. This will suspend the process until a connection is
    available. *)

val local_addr : t -> Kernel.Net.Addr.stream_addr
(** Get the local address the listener is bound to *)

val close : t -> unit
(** Close the listener *)
