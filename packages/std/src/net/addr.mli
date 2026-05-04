open Global

(**
   Network address handling.

   ## Example

   ```ocaml
   let addr = Addr.from_host_and_port ~host:"127.0.0.1" ~port:8080 |> Result.unwrap
   let host = Addr.ip addr
   let port = Addr.port addr
   ```
*)
type 't raw_addr = Kernel.Net.SocketAddr.t
(** TCP address family tag. *)
type tcp_addr = Kernel.Net.IpAddr.t
(** Stream socket address. *)
type stream_addr = Kernel.Net.SocketAddr.t
(** Datagram socket address. *)
type datagram_addr = Kernel.Net.SocketAddr.t
(** Any network socket address. *)
type socket_addr = Kernel.Net.SocketAddr.t
(** Errors returned while parsing or constructing addresses. *)
type error =
  | System_error of IO.error
  | Invalid_port_number of string
  | Invalid_format of string

(** The loopback TCP address family. *)
val loopback: tcp_addr

(** Build a stream address from a TCP address family tag and port. *)
val tcp: tcp_addr -> int -> stream_addr

(** Build a datagram address from a TCP address family tag and port. *)
val udp: tcp_addr -> int -> datagram_addr

(** Build a stream address from a host string and port number. *)
val from_host_and_port: host:string -> port:int -> (stream_addr, error) Kernel.result

(** Build a datagram address from a host string and port number. *)
val from_host_and_port_datagram: host:string -> port:int -> (datagram_addr, error) Kernel.result

(** Parse a string like ["127.0.0.1:8080"] into a stream address. *)
val parse: string -> (stream_addr, error) Kernel.result

(** Parse a string like ["127.0.0.1:8080"] into a datagram address. *)
val parse_datagram: string -> (datagram_addr, error) Kernel.result

(** Extract the IP or host portion of a socket address. *)
val ip: socket_addr -> string

(** Extract the port portion of a socket address. *)
val port: socket_addr -> int
