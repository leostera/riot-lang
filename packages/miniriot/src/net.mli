(** Network I/O operations for Miniriot

    This module provides actor-friendly networking operations that integrate
    with Miniriot's scheduler and I/O polling. All blocking operations will
    properly suspend the calling process until I/O is ready. *)

type error = [ `Connection_refused | `Closed | `System_error of string ]

module Addr : sig
  (** Network addresses *)

  type 't raw_addr = 't Gluon.Net.Addr.raw_addr
  type tcp_addr = Gluon.Net.Addr.tcp_addr
  type stream_addr = Gluon.Net.Addr.stream_addr

  val loopback : tcp_addr
  val tcp : tcp_addr -> int -> stream_addr
  val of_host_and_port : host:string -> port:int -> (stream_addr, error) result
  val parse : string -> (stream_addr, error) result
  val ip : stream_addr -> string
  val port : stream_addr -> int
end

module TcpStream : sig
  (** TCP stream for connected sockets *)

  type t

  val connect : Addr.stream_addr -> (t, error) result
  (** Connect to a TCP endpoint. This will suspend the process until the
      connection is established. *)

  val read : t -> bytes -> ?pos:int -> ?len:int -> unit -> (int, error) result
  (** Read data from the stream. This will suspend the process until data is
      available. Returns the number of bytes read. *)

  val write : t -> bytes -> ?pos:int -> ?len:int -> unit -> (int, error) result
  (** Write data to the stream. This will suspend the process until the socket
      is ready for writing. Returns the number of bytes written. *)

  val close : t -> unit
  (** Close the stream *)
end

module TcpListener : sig
  (** TCP listener for accepting connections *)

  type t

  val bind :
    ?reuse_addr:bool ->
    ?reuse_port:bool ->
    ?backlog:int ->
    Addr.stream_addr ->
    (t, error) result
  (** Create and bind a TCP listener. The socket is automatically set to
      non-blocking mode. *)

  val accept : t -> (TcpStream.t * Addr.stream_addr, error) result
  (** Accept a connection. This will suspend the process until a connection is
      available. *)

  val close : t -> unit
  (** Close the listener *)
end
