(**
   TLS stream for encrypted connections over any transport

   TlsStream provides TLS encryption over abstract reader/writer pairs,
   making it transport-agnostic. It works with TCP sockets, Unix domain
   sockets, pipes, or any other byte stream.

   ## Examples

   HTTPS client:

   {[
     open Std

     let fetch_https url =
       let uri = Net.Uri.parse url |> Result.expect ~msg:"Invalid URL" in
       let host = Net.Uri.host uri |> Option.expect ~msg:"No host" in
       let port = Net.Uri.port uri |> Option.unwrap_or ~default:443 in

       let addr = Net.Addr.of_host_and_port ~host ~port
                  |> Result.expect ~msg:"Invalid address" in

       (* Connect TCP *)
       let tcp = Net.TcpStream.connect addr
                 |> Result.expect ~msg:"Connection failed" in

       (* Wrap in TLS *)
       let tls = Net.TlsStream.from_tcp_client ~hostname:host tcp
                 |> Result.expect ~msg:"TLS handshake failed" in

       (* Use reader/writer for generic I/O *)
       let reader = Net.TlsStream.to_reader tls in
       let writer = Net.TlsStream.to_writer tls in

       IO.write_all writer ~buf:"GET / HTTP/1.1\r\n\r\n"
       |> Result.expect ~msg:"Write failed";

       let buf = Bytes.create 4096 in
       match IO.read reader buf with
       | Ok n -> Bytes.sub_string buf 0 n
       | Error e -> failwith "Read failed"
   ]}

   HTTPS server:

   {[
     let run_server () =
       let addr = Net.Addr.(tcp loopback 8443) in
       let listener = Net.TcpListener.bind ~reuse_addr:true addr
                      |> Result.expect ~msg:"Bind failed" in

       let rec accept_loop () =
         match Net.TcpListener.accept listener with
         | Ok (tcp, client_addr) ->
             spawn (fun () ->
               match Net.TlsStream.from_tcp_server
                       ~cert_path:(Path.v "cert.pem")
                       ~key_path:(Path.v "key.pem")
                       tcp with
               | Ok tls -> handle_client tls
               | Error e -> Log.error "TLS handshake failed"
             ) |> ignore;
             accept_loop ()
         | Error e -> Log.error "Accept failed"
       in
       accept_loop ()
   ]}

   Transport-agnostic usage:

   {[
     (* TLS works over ANY reader/writer pair *)
     let add_tls reader writer ~hostname =
       Net.TlsStream.from_client_io ~reader ~writer ~hostname ()
   ]}
*)
open Global

(**
   TLS stream wrapping a reader/writer pair.

   The type parameters are:
   - ['src] represents the underlying transport source
   - ['err] represents errors from the underlying transport
*)

(** TLS-specific errors *)
type 'src t
type error =
  | Closed
  | Handshake_failed of string
  | System_error of IO.error
  | Network_read_failed of IO.error
  | Network_write_failed of IO.error
  | Tls_not_available
  | Unsupported_vectored_operation
type mode =
  | Client of string
  | Server of Path.t * Path.t

(**
   Create TLS client from any reader/writer pair.

   This performs the TLS handshake, which may suspend the calling process
   multiple times as it reads/writes to the underlying transport.

   @param reader Source of encrypted bytes (from network)
   @param writer Destination for encrypted bytes (to network)
   @param hostname Server hostname for SNI and certificate verification
*)
val from_client_io:
  reader:IO.Reader.t ->
  writer:IO.Writer.t ->
  hostname:string ->
  unit ->
  (Tcp_stream.t t, error) Kernel.result

(**
   Create TLS server from any reader/writer pair.

   @param cert_path Path to server certificate (PEM format)
   @param key_path Path to server private key (PEM format)
*)
val from_server_io:
  reader:IO.Reader.t ->
  writer:IO.Writer.t ->
  cert_path:Path.t ->
  key_path:Path.t ->
  unit ->
  (Tcp_stream.t t, error) Kernel.result

(**
   Create TLS stream from TCP socket.

   This is the core TCP wrapper that handles both client and server modes.
   Internally converts the socket to reader/writer pairs.

   @param mode Either [Client hostname] for client-side TLS with SNI,
               or [Server (cert_path, key_path)] for server-side TLS
*)
val from_tcp_socket: mode:mode -> Tcp_stream.t -> (Tcp_stream.t t, error) Kernel.result

(**
   Create TLS client from TCP stream.

   Convenience wrapper around [from_client_io] for TCP sockets.
*)
val from_tcp_client: hostname:string -> Tcp_stream.t -> (Tcp_stream.t t, error) Kernel.result

(**
   Create TLS server from TCP stream.

   Convenience wrapper around [from_server_io] for TCP sockets.
*)
val from_tcp_server:
  cert_path:Path.t ->
  key_path:Path.t ->
  Tcp_stream.t ->
  (Tcp_stream.t t, error) Kernel.result

(**
   Convert TLS stream to a generic Reader.

   Reads return plaintext data decrypted from the underlying stream.
   The calling process will be suspended if the underlying transport
   would block.

   Example:
   {[
     let tls = Net.TlsStream.from_tcp_client ~hostname:"example.com" tcp in
     let reader = Net.TlsStream.to_reader tls in

     let buf = Bytes.create 4096 in
     match IO.read reader buf with
     | Ok n -> process_plaintext (Bytes.sub buf 0 n)
     | Error Closed -> handle_closed ()
     | Error e -> handle_error e
   ]}
*)
val to_reader: 'src t -> IO.Reader.t

(**
   Convert TLS stream to a generic Writer.

   Writes encrypt plaintext and send it to the underlying stream.
   The calling process will be suspended if the underlying transport
   would block.

   Example:
   {[
     let tls = Net.TlsStream.from_tcp_client ~hostname:"example.com" tcp in
     let writer = Net.TlsStream.to_writer tls in

     let* () = IO.write_all writer ~buf:"Hello, world!" in
     IO.flush writer
   ]}
*)
val to_writer: 'src t -> IO.Writer.t

(**
   Get negotiated ALPN protocol (e.g., "h2", "http/1.1").

   Returns [None] if no ALPN was negotiated.
*)
val alpn_protocol: 'src t -> string option

(**
   Close the TLS stream.

   Note: This does not close the underlying transport - that's the
   caller's responsibility.
*)
val close: 'src t -> unit
