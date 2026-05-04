(**
   TCP listener for accepting incoming connections.

   ## Example

   ```ocaml
   open Std

   let addr = Kernel.Net.Addr.from_string "127.0.0.1:9000" |> Result.unwrap in
   match Tcp_listener.bind addr with
   | Ok listener ->
       (match Tcp_listener.accept listener with
       | Ok (_stream, _peer) -> Log.info "accepted connection"
       | Error _ -> Log.error "accept failed");
       Tcp_listener.close listener
   | Error _ ->
       Log.error "failed to bind listener"
   ```
*)
open Global

type t
(** Errors returned by listener operations. *)
type error =
  | Connection_refused
  | Closed
  | System_error of IO.error

(**
   Create and bind a TCP listener. The socket is automatically set to
   non-blocking mode.
*)
val bind:
  ?reuse_addr:bool ->
  ?reuse_port:bool ->
  ?backlog:int ->
  Addr.stream_addr ->
  (t, error) Kernel.result

(**
   Accept a connection. This will suspend the process until a connection is
   available. Optionally specify a timeout for the syscall.
*)
val accept:
  ?timeout:Time.Duration.t ->
  t ->
  (Kernel.Net.TcpStream.t * Addr.stream_addr, error) Kernel.result

(** Get the local address the listener is bound to. *)
val local_addr: t -> Addr.stream_addr

(** Close the listener. *)
val close: t -> unit
