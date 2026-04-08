open Global0

type 't raw_addr = string
type tcp_addr =
  ([
    `v4
    | `v6
  ]) raw_addr
type stream_addr =
[
  `Tcp of tcp_addr * int
]
type datagram_addr =
[
  `Udp of tcp_addr * int
]
type socket_addr = [
  stream_addr
  | datagram_addr
]
module Ipaddr: sig
  val to_unix: tcp_addr -> Unix.inet_addr

  val of_unix: Unix.inet_addr -> tcp_addr
end

val loopback: tcp_addr

val tcp: string -> int -> stream_addr

val udp: string -> int -> datagram_addr

val to_unix: [<
    socket_addr
  ] -> Unix.socket_type * Unix.sockaddr

val to_domain: [<
    socket_addr
  ] -> Unix.socket_domain

val of_unix: Unix.sockaddr -> stream_addr

val of_unix_datagram: Unix.sockaddr -> datagram_addr

val to_string: 'a raw_addr -> string

val of_addr_info: Unix.addr_info -> stream_addr option

val of_addr_info_datagram: Unix.addr_info -> datagram_addr option

val of_host_and_port: host:string -> port:int -> (stream_addr, IO.error) result

val of_host_and_port_datagram: host:string -> port:int -> (datagram_addr, IO.error) result

val get_info: stream_addr -> (stream_addr list, IO.error) result

val get_info_datagram: datagram_addr -> (datagram_addr list, IO.error) result

val ip: [<
    socket_addr
  ] -> tcp_addr

val port: [<
    socket_addr
  ] -> int
