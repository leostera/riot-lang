open Global

(** UDP server convenience wrapper built on {!Net.UdpSocket}.

    UDP servers receive independent packets rather than accepted connections, so
    the handler is invoked once per datagram and can reply through the shared
    bound socket.

    ## Example

    ```ocaml
    open Std

    let handler ~socket ~from payload ~len =
      ignore len;
      ignore (Net.UdpSocket.send_to socket from payload ())

    let addr = Net.Addr.udp "127.0.0.1" 9000 in
    ignore (Net.UdpServer.listen addr ~handler)
    ```
*)
type t
(** Errors returned by UDP server operations. *)
type error =
  | System_error of IO.error
(** Datagram handler invoked for each received packet. The [payload] bytes are
    trimmed to the datagram length. *)
type handler = socket:Udp_socket.t -> from:Addr.datagram_addr -> bytes -> len:int -> unit

(** Bind a UDP server socket and remember the packet handler that should run for
    each datagram. *)
val bind:
  ?reuse_addr:bool ->
  ?reuse_port:bool ->
  ?buffer_size:int ->
  Addr.datagram_addr ->
  handler:handler ->
  (t, error) result

(** Run the receive loop until an error occurs. Each datagram is dispatched in
    its own actor so the loop can continue receiving while handlers run. *)
val serve: t -> (unit, error) result

(** Convenience wrapper around {!bind} followed by {!serve}. *)
val listen:
  ?reuse_addr:bool ->
  ?reuse_port:bool ->
  ?buffer_size:int ->
  Addr.datagram_addr ->
  handler:handler ->
  (unit, error) result

(** Return the local address the server socket is bound to. *)
val local_addr: t -> Addr.datagram_addr

(** Return the shared UDP socket backing the server. *)
val socket: t -> Udp_socket.t

(** Close the server socket. *)
val close: t -> unit
