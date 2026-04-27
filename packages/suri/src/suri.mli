open Std

(**
   {1 Suri - High-Performance Web Framework for OCaml}

   Suri is an experimental, actor-based web framework built on {!Std} and
   {!Runtime}. It provides the framework pieces for building typed HTTP
   applications while protocol, security, and operational hardening continue.

   {2 Table of Contents}

   - {{!section-why}Why Suri?}
   - {{!section-quickstart}Quick Start}
   - {{!section-modules}Module Overview}
   - {{!section-examples}Examples}
   - {{!section-architecture}Architecture}

   {2:why Why Suri?}

   {b ✅ Actor-Based Concurrency}
   - Built on Std.Runtime's lightweight processes
   - Supervised connection pools with automatic restart
   - Handle thousands of concurrent connections efficiently

   {b ✅ Type-Safe Components}
   - React-style component system for building UIs
   - Static HTML rendering with experimental LiveView integration
   - No inline JavaScript required

   {b ✅ Composable Middleware}
   - Router with parameter extraction
   - Pipeline-based request processing
   - Easy to write custom middleware

   {b Current Status}
   - HTTP/1.1 server foundation with keep-alive support
   - WebSocket support via Channel API is experimental
   - LiveView, sessions, CSRF, and HTTP/2 are still being hardened
   - Do not treat Suri as production-ready yet

   {2:quickstart Quick Start}

   {3 Hello World}

   The simplest possible Suri server - just a list of middleware!

   {[
     open Std
     open Suri

     let app = [
       (fun conn -> Conn.respond conn ~status:Ok ~body:"Hello, World!")
     ]

     let main ~args:_ =
       match Suri.start_link app with
       | Ok _ ->
           Log.info "Server running on http://0.0.0.0:4000";
           let rec loop () = sleep (Time.Duration.from_secs 100); loop () in
           loop ()
       | Error _ ->
           Error (Failure "Failed to bind")

     let () = Runtime.run ~main ~args:Env.args ()
   ]}

   {3 With Custom Port}

   {[
     let config = Suri.config ~port:8080 () in
     Suri.start_link ~config app
   ]}

   {3 With Routing and Middleware}

   Compose middleware as a simple list:

   {[
     open Std
     open Suri

     (* Custom middleware *)
     let logger conn =
       Log.info ("Request: " ^ Conn.uri conn);
       conn

     let cors conn =
       Conn.with_header conn "Access-Control-Allow-Origin" "*"

     (* Routes *)
     let routes = Middleware.Router.[
       get "/" (fun conn -> Conn.respond conn ~status:Ok ~body:"Home");
       get "/about" (fun conn -> Conn.respond conn ~status:Ok ~body:"About");
     ]

     (* App is just a list! *)
     let app = [
       logger;
       cors;
       Middleware.Router.middleware routes;
     ]

     let main ~args:_ =
       match Suri.start_link app with
       | Ok _ ->
           Log.info "Server running";
           let rec loop () = sleep (Time.Duration.from_secs 100); loop () in
           loop ()
       | Error _ ->
           Error (Failure "Failed to bind")

     let () = Runtime.run ~main ~args:Env.args ()
   ]}

   {3 Custom Port}

   Override defaults with optional parameters:

   {[
     let config = Suri.config ~port:8080 () in
     Suri.start_link ~config ~handler ()
   ]}

   {3 With Routing}

   Use middleware for routing - it's just a list of functions!

   {[
     open Std
     open Suri

     (* Define your middleware *)
     let logger conn =
       Log.info ("Request: " ^ Middleware.Conn.uri conn);
       conn

     let routes = Middleware.Router.[
       get "/" (fun conn -> Middleware.Conn.respond conn ~status:Ok ~body:"Home");
       get "/about" (fun conn -> Middleware.Conn.respond conn ~status:Ok ~body:"About");
     ]

     (* Pipeline is just a list! *)
     let app = [
       logger;
       Middleware.Router.middleware routes;
     ]

     (* Handler runs the pipeline *)
     let handler socket_conn req =
       let conn = Middleware.Conn.make socket_conn req in
       let conn = Middleware.Pipeline.run conn app in
       close (Middleware.Conn.to_response conn)

     let main ~args:_ =
       match Suri.start_link ~handler () with
       | Ok _ ->
           Log.info "Server with routing on http://0.0.0.0:4000";
           let rec loop () = sleep (Time.Duration.from_secs 100); loop () in
           loop ()
       | Error _ ->
           Error (Failure "Failed to bind")

     let () = Runtime.run ~main ~args:Env.args ()
   ]}

   {3 Type-Safe Components}

   {[
     open Std
     open Suri
     open Suri.Component

     let welcome_page : unit t =
       html [
         head [
           title_ [text "Welcome"];
           meta ~attrs:[attr "charset" "UTF-8"] ();
         ];
         body [
           div ~attrs:[class_ "container"] [
             h1 [text "Welcome to Suri"];
             p [text "Build type-safe web apps with OCaml"];
             button ~attrs:[class_ "btn"] [text "Get Started"];
           ];
         ];
       ]

     let handler _conn _req =
       let html = to_html welcome_page in
       WebServer.Response.ok
         ~headers:(Http.Header.of_list [("Content-Type", "text/html")])
         ~body:html
         ()
   ]}

   {2:modules Module Overview}

   {3 Core Server Modules}

   - {!SocketPool} - Low-level TCP connection pool with protocol abstraction
   - {!WebServer} - HTTP/1.1 server with request/response handling
   - {!Middleware} - Composable middleware pipeline and routing
   - {!Channel} - WebSocket handler abstraction

   {3 UI & Component Modules}

   - {!Component} - Type-safe HTML component system (static + LiveView)

   {2:examples Examples}

   All examples are available in [packages/suri/examples/].

   {3 Available Examples}

   - [hello_world.ml] - Minimal HTTP server
   - [routing.ml] - Router with middleware pipeline
   - [json_api.ml] - RESTful JSON API with parameter extraction
   - [basic_component.ml] - Full-page component example with forms
   - [design_system.ml] - Reusable component library pattern
   - [liveview_migration.ml] - Static HTML → LiveView migration guide

   {b Run an example:}
   {[
     riot run suri:hello_world
     riot run suri:routing
     riot run suri:basic_component
   ]}

   {2:architecture Architecture}

   {3 Supervision Tree}

   {v
     WebServer.Supervisor
       ├── SocketPool.Supervisor
       │   ├── Acceptor 1
       │   ├── Acceptor 2
       │   └── ... (configurable)
       └── Connection Handlers (dynamic)
   v}

   {3 Request Flow}

   {v
     TCP Accept → Parse HTTP → Middleware Pipeline → Handler → Send Response
         ↓            ↓              ↓                   ↓
     SocketPool   WebServer     Router/Logger      User Code
   v}

   {3 Component Rendering}

   {v
     Component Tree → to_html → Static HTML (events ignored)
                   ↓
                 LiveView → Interactive (events → server)
   v}

   {2 Performance Tips}

   {3 Tune Connection Pool}

   Increase acceptors for high concurrency:
   {[
     let config = WebServer.Config.make ~acceptors:200 ()
   ]}

   {3 Adjust Buffer Sizes}

   Larger buffers for big requests/responses:
   {[
     let config = WebServer.Config.make ~buffer_size:8192 ()
   ]}

   {3 Monitor Health}

   Use supervision API to monitor active connections:
   {[
     let count = Supervisor.Dynamic.count_children supervisor in
     Log.info "Active connections: %d" count.active
   ]}

   {2 Next Steps}

   - Read the examples in [packages/suri/examples/]
   - Explore the {!Component} module for UI building
   - Check out {!Middleware.Router} for routing patterns
   - See {!WebServer.Response} for response helpers

   ---

   {1 API Reference}
*)

(** {2 Top-Level API} *)

module Config: sig
  (**
     Server Configuration

     Compound configuration for the entire Suri server including
     network settings, HTTP limits, protocol-specific options, and
     LiveView session security.
  *)
  type env =
    | Development
    | Test
    | Production
  type t = {
    env: env;
    host: string;
    port: int;
    acceptors: int;
    max_request_line_length: int;
    max_header_count: int;
    max_header_length: int;
    max_body_size: int;
    max_keep_alive_requests: int;
    max_websocket_frame_size: int;
    max_websocket_message_size: int;
    read_header_timeout_ms: int;
    read_body_timeout_ms: int;
    idle_timeout_ms: int;
    write_timeout_ms: int;
    buffer_size: int;
    liveview_secret: string;
    (** Secret key for signing LiveView session tokens (min 32 characters) *)
  }
  val default: t

  (**
     Default configuration:
     - host: "0.0.0.0" (all interfaces)
     - port: 4000
     - acceptors: Thread.available_parallelism
     - max_request_line_length: 8192
     - max_header_count: 100
     - max_header_length: 8192
     - max_body_size: 10485760
     - max_keep_alive_requests: 100
     - max_websocket_frame_size: 1048576
     - max_websocket_message_size: 16777216
     - read_header_timeout_ms: 5000
     - read_body_timeout_ms: 30000
     - idle_timeout_ms: 60000
     - write_timeout_ms: 30000
     - buffer_size: 4096
     - liveview_secret: "INSECURE-CHANGE-ME-TO-AT-LEAST-32-CHARS" (MUST change in production!)
  *)

  (** Configuration via Std.Config - see Config.mli for full documentation *)
  val spec: Std.Config.Spec.t

  type liveview_secret_error =
    | Missing
    | TooShort of int
    | Placeholder
  type invalid_env = {
    value: string;
    normalized: string;
    allowed: env list;
  }
  type error =
    | InvalidEnv of invalid_env
    | InvalidPort of int
    | InvalidAcceptors of int
    | InvalidMaxRequestLineLength of int
    | InvalidMaxHeaderCount of int
    | InvalidMaxHeaderLength of int
    | InvalidMaxBodySize of int
    | InvalidMaxKeepAliveRequests of int
    | InvalidMaxWebSocketFrameSize of int
    | InvalidMaxWebSocketMessageSize of int
    | InvalidReadHeaderTimeoutMs of int
    | InvalidReadBodyTimeoutMs of int
    | InvalidIdleTimeoutMs of int
    | InvalidWriteTimeoutMs of int
    | InvalidBufferSize of int
    | InvalidLiveViewSecret of liveview_secret_error
  val env_to_string: env -> string

  val env_from_string: string -> (env, error) result

  val error_to_string: error -> string

  val errors_to_string: error list -> string

  val validate: t -> (t, error list) result

  val get: Std.Config.Spec.value -> (t, Std.Config.error) result
end

val config:
  ?env:Config.env ->
  ?host:string ->
  ?port:int ->
  ?acceptors:int ->
  ?max_request_line_length:int ->
  ?max_header_count:int ->
  ?max_header_length:int ->
  ?max_body_size:int ->
  ?max_keep_alive_requests:int ->
  ?max_websocket_frame_size:int ->
  ?max_websocket_message_size:int ->
  ?read_header_timeout_ms:int ->
  ?read_body_timeout_ms:int ->
  ?idle_timeout_ms:int ->
  ?write_timeout_ms:int ->
  ?buffer_size:int ->
  ?liveview_secret:string ->
  unit ->
  Config.t

(** Create server configuration with optional parameters. *)
(** {2 Core Types} *)

type middleware = Middleware.Pipeline.middleware
(** A middleware function: [Conn.t -> Conn.t] *)
type handler = Middleware.Pipeline.t
(** A handler is just a list of middleware functions *)
type start_error =
  | InvalidAddress of Std.Net.Addr.error
  | BindFailed of Std.Net.TcpListener.error
(** {2 Starting the Server} *)
val start_link: ?config:Config.t -> handler -> (Supervisor.Dynamic.t, start_error) result

(**
   Start a Suri web server with a middleware pipeline.

   Your application is simply a list of [Conn.t -> Conn.t] functions.
   Each middleware can transform the connection, set response data,
   or halt the pipeline.

   Examples:

   {[
     (* Minimal *)
     let app = [
       (fun conn -> Conn.respond conn ~status:Ok ~body:"Hello!")
     ]
     Suri.start_link app

     (* With middleware *)
     let app = [
       logger;
       auth;
       router;
     ]
     Suri.start_link app

     (* Custom config *)
     let config = Suri.config ~port:8080 () in
     Suri.start_link ~config app
   ]}

   @param config Server configuration (defaults to Suri.config())
   @param handler Middleware pipeline (list of Conn.t -> Conn.t)
   @return Ok supervisor_pid if successful, Error _ if startup fails
*)
(** {2 User-Facing Modules} *)

module Conn = Middleware.Conn

(**
   Connection type and transformations.

   This is your primary API for handling requests in middleware.

   Core functions:
   - [respond ~status ~body] - Set response
   - [with_header key value] - Add header
   - [with_body body] - Set body
   - [send] - Mark as sent (halts pipeline)
   - [uri], [method_], [headers] - Request accessors
   - [params] - Get route parameters

   See {!Middleware.Conn} for full API.
*)
module Response = Web_server.Response

(**
   HTTP Response builders.

   Most users should use [Conn.respond] in middleware, but [Response]
   is useful for building responses directly.

   See {!Web_server.Response} for full documentation.
*)
module Request = Web_server.Request

(**
   HTTP Request accessors.

   Most users should use [Conn] methods in middleware, but [Request]
   is useful for extracting request data.

   See {!Web_server.Request} for full documentation.
*)
(** {2 Framework Modules} *)

module Middleware = Middleware

(**
   Composable middleware framework.

   Includes:
   - {!Middleware.Conn} - Connection context
   - {!Middleware.Pipeline} - Middleware composition
   - {!Middleware.Router} - Pattern-based routing

   See {!Middleware} for complete documentation.
*)
module Component = Component

(**
   Type-safe HTML component system.

   Build UIs with React-style components that work with both
   static HTML generation and LiveView interactivity.

   Features:
   - 115+ HTML5 elements
   - 30+ attribute helpers
   - 15+ event handlers for LiveView
   - Conditional rendering helpers

   See {!Component} for complete documentation.
*)
module LiveView = Liveview

(**
   Server-rendered components with live updates.

   Phoenix LiveView-style interactive UIs where events are
   sent to the server over WebSocket and DOM patches are
   sent back to the client.

   See {!LiveView} for complete documentation.
*)
module Testing = Testing

(**
   Helpers for exercising real Suri applications and middleware in tests.

   See {!Testing} for request builders, app runners, middleware helpers, and
   response expectations.
*)
