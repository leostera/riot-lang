open Global

(** Network address handling.

    ## Example

    ```ocaml
    let addr = Addr.of_host_and_port ~host:"127.0.0.1" ~port:8080 |> Result.unwrap
    let host = Addr.ip addr
    let port = Addr.port addr
    ```
*)
type 't raw_addr = 't Kernel.Net.Addr.raw_addr
(** TCP address family tag. *)
type tcp_addr = Kernel.Net.Addr.tcp_addr
(** Stream socket address. *)
type stream_addr = Kernel.Net.Addr.stream_addr
(** Errors returned while parsing or constructing addresses. *)
type error =
  | System_error of IO.error
  | Invalid_port_number of string
  | Invalid_format of string

(** The loopback TCP address family. *)
val loopback: tcp_addr

(** Build a stream address from a TCP address family tag and port. *)
val tcp: tcp_addr -> int -> stream_addr

(** Build a stream address from a host string and port number. *)
val of_host_and_port: host:string -> port:int -> (stream_addr, error) result

(** Parse a string like ["127.0.0.1:8080"] into a stream address. *)
val parse: string -> (stream_addr, error) result

(** Extract the IP or host portion of a stream address. *)
val ip: stream_addr -> string

(** Extract the port portion of a stream address. *)
val port: stream_addr -> int
