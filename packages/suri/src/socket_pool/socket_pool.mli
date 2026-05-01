open Std

(**
   TCP connection pool with concurrent acceptors and handlers.

   SocketPool provides a high-performance TCP server architecture with:
   - Multiple acceptor processes for concurrent connection acceptance
   - Per-connection handler processes
   - Pluggable transport layer (TCP, TLS)
   - Connection lifecycle management

   ## Architecture

   ```
   SocketPool
     ↓
   Acceptor Pool (N processes)
     ↓
   Connector (1 process per connection)
     ↓
   Handler (your application logic)
   ```

   ## Example: Echo Server

   ```ocaml
   open Std

   open Suri

   module Echo_handler = struct
     include SocketPool.Handler.Default

     type state = unit
     type error =
       | SendFailed of SocketPool.Connection.error

     let handle_data data conn () =
       match SocketPool.Connection.send conn data with
       | Ok () -> Continue ()
       | Error SocketPool.Connection.Closed -> Close ()
       | Error error -> Error ((), SendFailed error)
   end

   let () =
     let _result =
       SocketPool.start_link ~port:8080 ~acceptors:10
         (module Echo_handler)
         ()
     in
     (* Keep process alive *)
     receive_any ()
   ```

   ## Example: HTTP Server Foundation

   ```ocaml
   module Http_handler = struct
     include SocketPool.Handler.Default

     type state = { requests : int }
     type error =
       | MalformedRequest
       | SendFailed of SocketPool.Connection.error

     let handle_connection _conn state =
       Continue { requests = 0 }

     let handle_data data conn state =
       (* Parse HTTP request from data *)
       let response = "HTTP/1.1 200 OK\r\n\r\nHello World!" in
       match SocketPool.Connection.send conn response with
       | Ok () -> Continue { requests = state.requests + 1 }
       | Error SocketPool.Connection.Closed -> Close state
       | Error error -> Error (state, SendFailed error)
   end

   let () =
     SocketPool.start_link ~port:3000 ~acceptors:100
       (module Http_handler)
       { requests = 0 }
   ```
*)
module Connection: module type of Connection

(** Connection management - see {!Connection} *)
module Handler: module type of Handler

(** Handler abstraction - see {!Handler} *)
module Transport: module type of Transport

(** Transport layer - see {!Transport} *)
type error =
  | InvalidAddress of Net.Addr.error
  | BindFailed of Net.TcpListener.error
  | InvalidAcceptors of int
  | InvalidBufferSize of int

val validate_start_options: acceptors:int -> buffer_size:int -> (unit, error) result

val start_link:
  host:string ->
  port:int ->
  ?acceptors:int ->
  ?buffer_size:int ->
  ?transport:Transport.t ->
  ('state, 'err) Handler.handler ->
  'state ->
  (Supervisor.Dynamic.t, error) result

(**
   [start_link ~host ~port handler initial_state] starts a supervised TCP server.

   - [host] - Host to bind to (e.g., "0.0.0.0" or "127.0.0.1")
   - [port] - Port to listen on
   - [acceptors] - Number of concurrent acceptor processes (default 100)
   - [buffer_size] - Read buffer size per connection (default 4096)
   - [transport] - Transport layer (default plain TCP)
   - [handler] - Handler module implementing connection logic
   - [initial_state] - Initial state for new connections

   Returns [Ok supervisor_pid] with a dynamic supervisor managing acceptors,
   or [Error _] if address resolution or port binding failed.

   The server uses a {!Std.Supervisor.Dynamic} to manage [acceptors] processes
   that concurrently accept connections. Each accepted connection spawns a new
   Connector process that runs the handler logic. If acceptors crash, they are
   automatically restarted by the supervisor.

   Example:
   ```ocaml
   let result = SocketPool.start_link
     ~host:"0.0.0.0"
     ~port:8080
     ~acceptors:50
     (module My_handler)
     { my_initial_state }

   match result with
   | Ok supervisor ->
       Log.info "Server started with supervisor";
       (* Keep process alive or link supervisor to your app supervisor *)
       ()
   | Error _ ->
       Log.error "Failed to bind to port"
   ```
*)
