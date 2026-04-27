type t
type error =
  | InvalidSlice of { pos: int; len: int; buffer_len: int }
  | InvalidSocketAddr of { ip: string; port: int }
  | WouldBlock
  | TimedOut
  | ConnectionRefused
  | ConnectionReset
  | NetworkUnreachable
  | NotConnected
  | MessageTooLong
  | DestinationAddressRequired
  | AddressInUse
  | AddressNotAvailable
  | System of System_error.t
val error_to_string: error -> string

(** Use `bind addr` to create a nonblocking UDP socket immediately. *)
val bind: ?reuse_addr:bool -> ?reuse_port:bool -> Socket_addr.t -> (t, error) Result.t

(**
   Use `connect socket addr` to set the default peer for `send`, `recv`, and `recv_from`.

   After a socket is connected, datagrams from other peers are filtered out. This behavior is
   part of the contract on both IPv4 and IPv6 loopback.
*)
val connect: t -> Socket_addr.t -> (unit, error) Result.t

val close: t -> (unit, error) Result.t

(** Use `local_addr socket` to inspect the bound local address immediately. *)
val local_addr: t -> (Socket_addr.t, error) Result.t

(** On a connected socket, `recv` observes only traffic from the connected peer. *)
val recv: t -> ?pos:int -> ?len:int -> bytes -> (int, error) Result.t

(**
   On a connected socket, `recv_from` observes only traffic from the connected peer and still
   reports that peer's address.
*)
val recv_from: t -> ?pos:int -> ?len:int -> bytes -> (int * Socket_addr.t, error) Result.t

(**
   Use `send socket buf` only after a peer is connected.

   Unconnected sockets report `DestinationAddressRequired` or `NotConnected`.
*)
val send: t -> ?pos:int -> ?len:int -> bytes -> (int, error) Result.t

val send_to: t -> Socket_addr.t -> ?pos:int -> ?len:int -> bytes -> (int, error) Result.t

(** Use `to_source socket` to expose readiness for nonblocking `recv`, `recv_from`, and `send`. *)
val to_source: t -> Async.Source.t
