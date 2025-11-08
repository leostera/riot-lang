open Global

(** Network address handling *)

type 't raw_addr = 't Kernel.Net.Addr.raw_addr
type tcp_addr = Kernel.Net.Addr.tcp_addr
type stream_addr = Kernel.Net.Addr.stream_addr

val loopback : tcp_addr
val tcp : tcp_addr -> int -> stream_addr

val of_host_and_port :
  host:string -> port:int -> (stream_addr, [> `System_error of string ]) result

val parse : string -> (stream_addr, [> `System_error of string ]) result
val ip : stream_addr -> string
val port : stream_addr -> int
