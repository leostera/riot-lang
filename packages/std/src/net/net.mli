(** # Net - Network I/O for actors

    Actor-friendly networking operations that integrate with Miniriot's
    scheduler. All blocking operations properly suspend the calling process
    until I/O is ready, allowing other actors to run.

    ## Examples

    TCP client:

    ```ocaml open Std open Miniriot

    let client_process () = let addr = Net.Addr.of_host_and_port
    ~host:"example.com" ~port:80 |> Result.expect ~msg:"Invalid address" in

    let stream = Net.TcpStream.connect addr |> Result.expect ~msg:"Connection
    failed" in

    let request = "GET / HTTP/1.0\r\n\r\n" in let buf = Bytes.of_string request
    in Net.TcpStream.write stream buf () |> ignore;

    let response = Bytes.create 4096 in match Net.TcpStream.read stream response
    () with | Ok n -> Log.info "Received %d bytes" n | Error e -> Log.error
    "Read failed";

    Net.TcpStream.close stream

    let () = spawn client_process |> ignore ```

    TCP server:

    ```ocaml let server_process () = let addr = Net.Addr.(tcp loopback 8080) in
    let listener = Net.TcpListener.bind ~reuse_addr:true addr |> Result.expect
    ~msg:"Failed to bind" in

    Log.info "Server listening on port 8080";

    let rec accept_loop () = match Net.TcpListener.accept listener with | Ok
    (stream, client_addr) -> Log.info "Client connected from %s:%d" (Net.Addr.ip
    client_addr) (Net.Addr.port client_addr); spawn (fun () -> handle_client
    stream) |> ignore; accept_loop () | Error e -> Log.error "Accept failed" in
    accept_loop ()

    and handle_client stream = (* Handle client... *) Net.TcpStream.close stream

    let () = spawn server_process |> ignore ```

    ## Key Features

    - **Non-blocking I/O**: All operations suspend the actor, not the scheduler
    - **Process-based concurrency**: Spawn an actor per connection
    - **Error handling**: Explicit Result types for all I/O operations
    - **Address parsing**: Type-safe network address handling

    ## When to Use

    - Building network servers (HTTP, TCP protocols)
    - Network clients that need to be concurrent
    - Any I/O-bound network application with many connections

    See [Uri] for URL parsing and [Http] for HTTP-specific functionality. *)

type error = [ `Connection_refused | `Closed | `System_error of string ]
(** Network error types. *)

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
