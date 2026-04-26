(**
   TCP client for line-based protocols.

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
         (match TcpClient.receive client with
         | Ok line -> println line
         | Error msg -> println ("Error: " ^ msg))
     | Error err -> println "Connection failed"
   ]}
*)
open Global

(** The client connection type. Contains the TCP stream and internal buffers. *)
type t
(** Errors returned by client operations. *)
type error =
  | Connection_refused
  | Closed
  | System_error of IO.error

(**
   [connect ~host ~port] establishes a TCP connection to the given host and
   port. Returns [Error] if the connection cannot be established.
*)
val connect: host:string -> port:int -> (t, error) Kernel.result

(**
   [send client data] sends the string data to the server. The string should
   include any necessary delimiters (e.g., newlines). The entire string will be
   sent before returning. Returns [Error] if the send fails.
*)
val send: t -> string -> (unit, string) Kernel.result

(**
   [receive client] reads from the server until a newline character is found,
   then returns the line (without the newline). If multiple lines were received
   in a single read, the additional data is buffered internally and will be
   returned by subsequent calls to [receive].

   This function blocks until a complete line is available or an error occurs.
   It can be called multiple times to handle streaming responses where each
   response is newline-delimited.
*)
val receive: t -> (string, string) Kernel.result

(** [close client] closes the TCP connection. *)
val close: t -> unit
