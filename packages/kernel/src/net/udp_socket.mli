open Global0

type t

val bind:
  ?reuse_addr:bool ->
  ?reuse_port:bool ->
  Addr.datagram_addr ->
  (t, IO.error) result

val connect: t -> Addr.datagram_addr -> (unit, IO.error) result

val close: t -> unit

val local_addr: t -> (Addr.datagram_addr, IO.error) result

val recv: t -> ?pos:int -> ?len:int -> bytes -> (int, IO.error) result

val recv_from: t -> ?pos:int -> ?len:int -> bytes -> ((int * Addr.datagram_addr), IO.error) result

val send: t -> ?pos:int -> ?len:int -> bytes -> (int, IO.error) result

val send_to: t -> Addr.datagram_addr -> ?pos:int -> ?len:int -> bytes -> (int, IO.error) result

val to_source: t -> Async.Source.t
