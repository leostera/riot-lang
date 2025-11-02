type 't raw_addr = string
type tcp_addr = [ `v4 | `v6 ] raw_addr
type stream_addr = [ `Tcp of tcp_addr * int ]

module Ipaddr : sig
  val to_unix : tcp_addr -> Unix.inet_addr
  val of_unix : Unix.inet_addr -> tcp_addr
end

val loopback : tcp_addr
val tcp : string -> int -> stream_addr
val to_unix : stream_addr -> Unix.socket_type * Unix.sockaddr
val to_domain : stream_addr -> Unix.socket_domain
val of_unix : Unix.sockaddr -> stream_addr
val pp : Format.formatter -> stream_addr -> unit
val to_string : 'a -> 'a
val of_addr_info : Unix.addr_info -> stream_addr option

val of_host_and_port :
  host:string -> port:int -> (stream_addr, IO.error) result

val get_info :
  stream_addr -> (stream_addr list, IO.error) result

val ip : stream_addr -> tcp_addr
val port : stream_addr -> int
