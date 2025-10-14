(** TCP connection pool with concurrent acceptors and handlers.

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
    open Miniriot
    open Suri

    module Echo_handler = struct
      include SocketPool.Handler.Default

      type state = unit
      type error = string

      let handle_data data conn () =
        match SocketPool.Connection.send conn data with
        | Ok () -> Continue ()
        | Error `Closed -> Close ()
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
      type error = [ `Parse_error of string ]

      let handle_connection _conn state =
        Continue { requests = 0 }

      let handle_data data conn state =
        (* Parse HTTP request from data *)
        let response = "HTTP/1.1 200 OK\r\n\r\nHello World!" in
        match SocketPool.Connection.send conn response with
        | Ok () -> Continue { requests = state.requests + 1 }
        | Error `Closed -> Close state
    end

    let () =
      SocketPool.start_link ~port:3000 ~acceptors:100
        (module Http_handler)
        { requests = 0 }
    ```
*)

module Connection : module type of Connection
(** Connection management - see {!Connection} *)

module Handler : module type of Handler
(** Handler abstraction - see {!Handler} *)

module Transport : module type of Transport
(** Transport layer - see {!Transport} *)

val start_link :
  port:int ->
  ?acceptors:int ->
  ?buffer_size:int ->
  ?transport:Transport.t ->
  (module Handler.Intf with type state = 'state and type error = 'err) ->
  'state ->
  (unit, [> `Bind_error ]) result
(** [start_link ~port handler initial_state] starts a TCP server.
    
    - [port] - Port to listen on
    - [acceptors] - Number of concurrent acceptor processes (default 100)
    - [buffer_size] - Read buffer size per connection (default 4096)
    - [transport] - Transport layer (default plain TCP)
    - [handler] - Handler module implementing connection logic
    - [initial_state] - Initial state for new connections
    
    Returns [Ok ()] if server started successfully,
    [Error `Bind_error] if port binding failed.
    
    The server spawns [acceptors] processes that concurrently accept
    connections. Each accepted connection spawns a new Connector process
    that runs the handler logic.
    
    Example:
    ```ocaml
    let _result = SocketPool.start_link
      ~port:8080
      ~acceptors:50
      (module My_handler)
      { my_initial_state }
    ```
*)
