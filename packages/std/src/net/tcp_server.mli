open Global

(**
   TCP server that manages a listener and handles line-based protocols.

   ## Example

   ```ocaml
   open Std

   let handler ~req stream =
     ignore req;
     ignore stream

   let addr = Kernel.Net.Addr.from_string "127.0.0.1:9000" |> Result.unwrap in
   let _ = Tcp_server.listen addr ~handler in
   ()
   ```
*)
type t
(** Errors returned by server operations. *)
type error =
  | Connection_refused
  | Closed
  | System_error of IO.error
(**
   Request handler invoked for each accepted line of input. The [req]
   parameter does not include the trailing newline.
*)
type handler = req:string -> Kernel.Net.TcpStream.t -> unit

(**
   Create a TCP server with a bound listener and start accepting connections.
   This function blocks and runs the accept loop until an error occurs.
*)
val listen:
  ?reuse_addr:bool ->
  ?reuse_port:bool ->
  ?backlog:int ->
  Addr.stream_addr ->
  handler:handler ->
  (unit, error) Kernel.result

(** Close the server. *)
val close: t -> unit
