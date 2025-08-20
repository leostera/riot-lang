(** Miniriot - Minimal single-core actor runtime *)

module Runtime : sig
  (** Runtime support for reduction counting *)

  val reset_reductions : int -> unit
  (** Reset the reduction count to a new value *)

  val increment_reduction_count : unit -> unit
  (** Increment (actually decrement) the reduction count and yield if necessary.
      This function is automatically injected by the Riot-patched OCaml compiler
      at function applications and loop iterations. *)
end

module Pid : sig
  type t

  val equal : t -> t -> bool
  val compare : t -> t -> int
  val pp : Format.formatter -> t -> unit
  val to_string : t -> string
end

module Message : sig
  type t = ..
end

module Process : sig
  type exit_reason = Normal | Exception of exn
end

(** Example message type *)
type Message.t += Exit

val run : main:(unit -> Process.exit_reason) -> int
(** Run the main function as the initial process. Can only be called once per
    process - subsequent calls will raise Failure. *)

val shutdown : status:int -> unit
(** Gracefully shutdown the scheduler with the given exit status. This will stop
    all processes and exit the run loop. *)

val spawn : (unit -> Process.exit_reason) -> Pid.t
(** Spawn a new process *)

val self : unit -> Pid.t
(** Get the current process PID *)

val send : Pid.t -> Message.t -> unit
(** Send a message to a process *)

val yield : unit -> unit
(** Yield control to the scheduler *)

val receive_any : unit -> Message.t
(** Receive any message from the mailbox without filtering.

    This function will block until a message is available and return the first
    message in the mailbox. *)

val receive :
  selector:(Message.t -> [ `select of 'msg | `skip ]) -> unit -> 'msg
(** Receive a message using a selector function for pattern matching.

    The selector function examines each message and returns:
    - [`select msg] to select and return the processed message
    - [`skip] to skip this message and continue searching

    This enables exhaustive pattern matching without catch-all cases:

    {[
      let selector = function
        | MyMessage data -> `select (`my_message data)
        | OtherMessage -> `select `other_message
        | _ -> `skip
      in
      match receive ~selector () with
      | `my_message data -> handle_my_message data
      | `other_message -> handle_other ()
      (* No catch-all needed - the match is exhaustive *)
    ]}

    The advantage of this pattern is that the selector narrows the return type,
    making the pattern match exhaustive and eliminating the need for a wildcard
    case that could hide bugs. *)

val exit : unit -> Process.exit_reason
(** Exit normally *)

val sleep : int -> unit
(** Sleep in milliseconds (currently just yields) *)

val enable_trace : unit -> unit
(** Enable debug tracing *)

val disable_trace : unit -> unit
(** Disable debug tracing *)

(** File I/O operations *)
module File : sig
  type error =
    [ `File_not_found
    | `Permission_denied
    | `Is_a_directory
    | `Not_a_directory
    | `Already_exists
    | `No_space
    | `Unknown of string ]

  val read : path:string -> (string, error) result
  (** Read the entire contents of a file *)

  val write : path:string -> content:string -> (unit, error) result
  (** Write a string to a file *)

  val exists : path:string -> bool
  (** Check if a file exists *)

  val remove : path:string -> (unit, error) result
  (** Remove a file *)

  val list_dir : path:string -> (string list, error) result
  (** List files and directories in a directory (excluding . and ..) *)

  val list_dir_all : path:string -> (string list, error) result
  (** List all files and directories in a directory (alias for list_dir) *)

  val is_directory : path:string -> bool
  (** Check if a path is a directory *)
end

(** Network I/O operations *)
module Net : sig
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

    val of_host_and_port :
      host:string -> port:int -> (stream_addr, error) result

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

    val write :
      t -> bytes -> ?pos:int -> ?len:int -> unit -> (int, error) result
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
    (** Handler receives request string (line without newline) and stream for responses *)
    val create : 
      ?reuse_addr:bool ->
      ?reuse_port:bool ->
      ?backlog:int ->
      Addr.stream_addr ->
      handler:handler ->
      (t, error) result
    (** Create a TCP server with a bound listener *)
    val listen : t -> (unit, [> `Server_stopped | `Read_error | `Connection_closed | error]) result
    (** Accept a connection, read a line, and call the handler *)
    val send : TcpStream.t -> string -> (unit, string) result
    (** Send a string to a stream - helper for handlers *)
    val stop : t -> unit
    (** Stop accepting new connections and close the listener *)
  end

  module TcpClient : sig
    (** TCP client for line-based protocols.
        
        This module provides a simple TCP client that handles line-based protocols
        (where messages are delimited by newlines). It properly buffers data to
        handle cases where multiple messages arrive in a single read, or where
        a message spans multiple reads.
        
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
        ]}
    *)
    
    type t
    (** The client connection type. Contains the TCP stream and internal buffers. *)
    
    val connect : host:string -> port:int -> (t, error) result
    (** [connect ~host ~port] establishes a TCP connection to the given host and port.
        Returns [Error] if the connection cannot be established. *)
    
    val send : t -> string -> (unit, string) result
    (** [send client data] sends the string data to the server. The string should
        include any necessary delimiters (e.g., newlines). The entire string will
        be sent before returning. Returns [Error] if the send fails. *)
    
    val receive : t -> (string, string) result
    (** [receive client] reads from the server until a newline character is found,
        then returns the line (without the newline). If multiple lines were received
        in a single read, the additional data is buffered internally and will be
        returned by subsequent calls to [receive]. 
        
        This function blocks until a complete line is available or an error occurs.
        It can be called multiple times to handle streaming responses where each
        response is newline-delimited. *)
    
    val close : t -> unit
    (** [close client] closes the TCP connection. *)
  end
end
