(**
   UDP socket for datagram-oriented networking.

   UDP preserves datagram boundaries. Each receive call returns at most one
   packet.

   ## Example

   ```ocaml
   open Std

   let socket =
     Net.UdpSocket.bind (Net.Addr.udp "127.0.0.1" 0)
     |> Result.expect ~msg:"bind failed"
   in
   let remote =
     Net.Addr.from_host_and_port_datagram ~host:"127.0.0.1" ~port:9000
     |> Result.expect ~msg:"invalid remote address"
   in
   let payload = Bytes.from_string "ping" in
   ignore (Net.UdpSocket.send_to socket remote payload ());
   Net.UdpSocket.close socket
   ```

   When the provided buffer is smaller than the incoming datagram, the payload
   is truncated to the buffer length by the operating system.
*)
open Global

type t = Kernel.Net.UdpSocket.t
(** Errors returned by UDP socket operations. *)
type error =
  | System_error of IO.error
(** Result returned when receiving from an unconnected UDP socket. *)
type recv_result = {
  bytes_read: int;
  from: Addr.datagram_addr;
}

(**
   Create and bind a UDP socket. The socket is automatically configured for
   non-blocking actor-friendly I/O.
*)
val bind: ?reuse_addr:bool -> ?reuse_port:bool -> Addr.datagram_addr -> (t, error) Kernel.result

(**
   Connect a UDP socket to a default remote peer. Connected sockets can use
   {!send} and {!recv} without specifying an address each time.
*)
val connect: t -> Addr.datagram_addr -> (unit, error) Kernel.result

(**
   Read one datagram from a connected UDP socket. This suspends the process
   until a packet is available.
*)
val recv:
  t ->
  bytes ->
  ?pos:int ->
  ?len:int ->
  ?timeout:Time.Duration.t ->
  unit ->
  (int, error) Kernel.result

(**
   Read one datagram together with its sender from an unconnected UDP socket.
   This suspends the process until a packet is available.
*)
val recv_from:
  t ->
  bytes ->
  ?pos:int ->
  ?len:int ->
  ?timeout:Time.Duration.t ->
  unit ->
  (recv_result, error) Kernel.result

(**
   Send one datagram to the connected peer. This suspends the process until
   the socket becomes writable.
*)
val send: t -> bytes -> ?pos:int -> ?len:int -> unit -> (int, error) Kernel.result

(**
   Send one datagram to an explicit destination. This suspends the process
   until the socket becomes writable.
*)
val send_to:
  t ->
  Addr.datagram_addr ->
  bytes ->
  ?pos:int ->
  ?len:int ->
  unit ->
  (int, error) Kernel.result

(** Return the local address the socket is bound to. *)
val local_addr: t -> Addr.datagram_addr

(** Close the socket. *)
val close: t -> unit
