(** Network I/O operations for Miniriot

    This module provides actor-friendly networking operations that integrate
    with Miniriot's scheduler and I/O polling. All blocking operations will
    properly suspend the calling process until I/O is ready. *)

type error = [ `Connection_refused | `Closed | `System_error of string ]

module Uri = Uri

module Addr : sig
  (** Network addresses *)

  type 't raw_addr
  type tcp_addr
  type stream_addr

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

module TcpServer : sig
  (** TCP server that manages a listener and handles line-based protocols *)

  type t

  type handler = req:string -> TcpStream.t -> unit
  (** Handler receives request string (line without newline) and stream for
      responses *)

  val listen :
    ?reuse_addr:bool ->
    ?reuse_port:bool ->
    ?backlog:int ->
    Addr.stream_addr ->
    handler:handler ->
    (t, error) result
  (** Create a TCP server with a bound listener *)
end

module TcpClient : sig
  (** TCP client for line-based protocols.

      This module provides a simple TCP client that handles line-based protocols
      (where messages are delimited by newlines). It properly buffers data to
      handle cases where multiple messages arrive in a single read, or where a
      message spans multiple reads.

      Example usage:
      {[
        let client = TcpClient.connect ~host:"localhost" ~port:8080 in
        match client with
        | Ok client ->
            (* Send a request *)
            let _ = TcpClient.send client "GET /status\n" in
            (* Receive response - blocks until newline *)
            let response = TcpClient.receive client in
            (* Can call receive multiple times for streaming responses *)
            let next_response = TcpClient.receive client in
            TcpClient.close client
        | Error e -> ...
      ]} *)

  type t
  (** The client connection type. Contains the TCP stream and internal buffers.
  *)

  val connect : host:string -> port:int -> (t, error) result
  (** [connect ~host ~port] establishes a TCP connection to the given host and
      port. Returns [Error] if the connection cannot be established. *)

  val send : t -> string -> (unit, string) result
  (** [send client data] sends the string data to the server. The string should
      include any necessary delimiters (e.g., newlines). The entire string will
      be sent before returning. Returns [Error] if the send fails. *)

  val receive : t -> (string, string) result
  (** [receive client] reads from the server until a newline character is found,
      then returns the line (without the newline). If multiple lines were
      received in a single read, the additional data is buffered internally and
      will be returned by subsequent calls to [receive].

      This function blocks until a complete line is available or an error
      occurs. It can be called multiple times to handle streaming responses
      where each response is newline-delimited. *)

  val close : t -> unit
  (** [close client] closes the TCP connection. *)
end
