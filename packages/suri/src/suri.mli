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
module For_testing: sig
  module Connection: sig
    type send_file_range_error = { off: int; len: int; size: int }
    type error =
      | Closed
      | FileError of Std.Fs.error
      | InvalidRange of send_file_range_error
    val write_all_with:
      write:(bytes -> pos:int -> len:int -> (int, 'error) Std.result) ->
      string ->
      (unit, error) Std.result

    val send_file_slice: ?off:int -> len:int -> string -> (string, error) Std.result
  end

  module Handler: sig
    val run_pipeline_response: Middleware.Pipeline.t -> Middleware.Conn.t -> Response.t option
  end

  module LiveViewSession: sig
    type decode_error =
      | InvalidTokenFormat
      | InvalidSignature
      | InvalidPayloadBase64
      | InvalidJson of Data.Json.error
    val sign: secret:string -> data:string -> string

    val verify: secret:string -> data:string -> signature:string -> bool

    val encode: secret:string -> json:Data.Json.t -> string

    val decode: secret:string -> token:string -> (Data.Json.t, decode_error) result

    val decode_error_to_string: decode_error -> string
  end

  module LiveViewProtocol: sig
    type client_msg =
      | Mount
      | Event of { handler_id: string; event_data: string }
    type client_msg_error =
      | InvalidJson of Data.Json.error
      | UnknownMessageFormat of Data.Json.t
      | UnexpectedDecodeException of exn
    val deserialize_client_msg: string -> (client_msg, client_msg_error) result

    val client_msg_error_to_string: client_msg_error -> string
  end

  module Channel: sig
    type initialization_error = ..
    type error =
      | InitializationFailed of initialization_error
      | UnknownOpcode of int
    type reported_error
    type ('state, 'error) result =
      | Continue of 'state
      | Push of Http.Ws.Frame.t list * 'state
      | Error of 'error
    val initialize: Channel.Handler.t -> (Channel.Handler.t, reported_error) result

    val reported_error: reported_error -> error

    val reported_error_to_string: reported_error -> string
  end

  module Http1: sig
    type serialization_error =
      | InvalidHeaderName of string
      | InvalidHeaderValue of { name: string; value: string }
    type websocket_key_error =
      | InvalidBase64
      | InvalidLength of { actual: int; expected: int }
    type websocket_upgrade_error =
      | InvalidWebSocketMethod of Std.Net.Http.Method.t
      | InvalidWebSocketVersion of Std.Net.Http.Version.t
      | MissingWebSocketUpgrade
      | InvalidWebSocketUpgrade of { value: string }
      | MissingWebSocketConnectionUpgrade
      | MissingWebSocketVersion
      | UnsupportedWebSocketVersion of { value: string; expected: string }
      | MissingWebSocketKey
      | InvalidWebSocketKey of { value: string; reason: websocket_key_error }
    type content_length_error =
      | InvalidInteger
      | NegativeLength of int
    type request_body_header_error =
      | InvalidContentLength of { value: string; reason: content_length_error }
      | ConflictingContentLength of {
          values: string list;
        }
      | TransferEncodingWithContentLength of {
          transfer_encoding: string;
          content_lengths: string list;
        }
      | UnsupportedTransferEncoding of { value: string }
    type request_header_error =
      | MissingHostHeader
    val serialize_response: Response.t -> (string, serialization_error) Std.result

    val compute_websocket_accept: string -> string

    val validate_websocket_upgrade: Request.t -> (string, websocket_upgrade_error) Std.result

    val websocket_upgrade_error_to_string: websocket_upgrade_error -> string

    val validate_request_body_headers:
      Std.Net.Http.Request.t ->
      (int, request_body_header_error) Std.result

    val request_body_header_error_to_string: request_body_header_error -> string

    val split_request_body: string -> int -> string * string

    val validate_request_headers: Std.Net.Http.Request.t -> (unit, request_header_error) Std.result

    val request_header_error_to_string: request_header_error -> string
  end
end
