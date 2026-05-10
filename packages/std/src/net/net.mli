(**
   Network I/O for actors.

   Actor-friendly networking operations that integrate with Std.Runtime's
   scheduler. All blocking operations properly suspend the calling process
   until I/O is ready, allowing other actors to run.

   ## Examples

   TCP client:

   ```ocaml open Std

   let client_process () = let addr = Net.Addr.from_host_and_port
   ~host:"example.com" ~port:80 |> Result.expect ~msg:"Invalid address" in

   let stream = Net.TcpStream.connect addr |> Result.expect ~msg:"Connection
   failed" in

   let request = "GET / HTTP/1.0\r\n\r\n" in let buf = Bytes.from_string request
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
   - **Process-based concurrency**: Spawn an actor per connection or datagram
   - **Error handling**: Explicit Result types for all I/O operations
   - **Address parsing**: Type-safe network address handling

   ## When to Use

   - Building network servers (HTTP, TCP, UDP protocols)
   - Network clients that need to be concurrent
   - Any I/O-bound network application with many connections

   See [Uri] for URL parsing and [Http] for HTTP-specific functionality.
*)

(** Network error types. *)
type error =
  | Connection_refused
  | Closed
  | System_error of IO.error

module Uri = Uri

(** Network addresses. *)
module Addr: module type of Addr

(** TCP stream for connected sockets. *)
module TcpStream: module type of Tcp_stream

(** Unix-domain stream for connected sockets. *)
module UnixStream: module type of Unix_stream

(** TCP listener for accepting connections. *)
module TcpListener: module type of Tcp_listener

(** TCP server that manages a listener and handles line-based protocols. *)
module TcpServer: module type of Tcp_server

(** UDP socket for datagram-oriented networking. *)
module UdpSocket: module type of Udp_socket

(** UDP server convenience wrapper for packet handlers. *)
module UdpServer: module type of Udp_server

(** TCP client for line-based protocols. *)
module TcpClient: module type of Tcp_client

(** TLS stream for encrypted connections. *)
module TlsStream: module type of Tls_stream

(** HTTP types and utilities. *)
module Http: module type of Http
