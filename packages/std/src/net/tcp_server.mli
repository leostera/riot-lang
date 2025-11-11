open Global

(** TCP server that manages a listener and handles line-based protocols *)

type t

type error =
  | Connection_refused
  | Closed
  | System_error of string

type handler = req:string -> Kernel.Net.Tcp_stream.t -> unit
(** Handler receives request string (line without newline) and stream for
    responses *)

val listen :
  ?reuse_addr:bool ->
  ?reuse_port:bool ->
  ?backlog:int ->
  Kernel.Net.Addr.stream_addr ->
  handler:handler ->
  (t, error) result
(** Create a TCP server with a bound listener *)

val accept_loop : t -> unit
(** Run the accept loop that handles incoming connections *)

val close : t -> unit
(** Close the server *)
