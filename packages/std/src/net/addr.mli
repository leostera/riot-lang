open Global

(** Network address handling *)
type 't raw_addr = 't Kernel.Net.Addr.raw_addr
type tcp_addr = Kernel.Net.Addr.tcp_addr
type stream_addr = Kernel.Net.Addr.stream_addr
type error =
  | System_error of IO.error
  | Invalid_port_number of string
  | Invalid_format of string
val loopback: tcp_addr

val tcp: tcp_addr -> int -> stream_addr

val of_host_and_port: host:string -> port:int -> (stream_addr, error) result

val parse: string -> (stream_addr, error) result

val ip: stream_addr -> string

val port: stream_addr -> int
