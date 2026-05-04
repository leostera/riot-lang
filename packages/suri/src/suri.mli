(**
   Experimental actor-based web framework for HTTP servers, middleware, components, and LiveView.

   Suri applications are middleware pipelines. Each middleware receives a
   `Conn.t`, updates request or response state, and returns the next connection.

   ```ocaml
   open Std
   open Suri

   let app = [
     (fun conn -> Conn.respond ~status:Net.Http.Status.Ok ~body:"Hello, World!" conn);
   ]

   let main ~args:_ =
     match Suri.start_link app with
     | Ok _supervisor ->
         let rec loop () =
           sleep (Time.Duration.from_secs 100);
           loop ()
         in
         loop ()
     | Error error ->
         Error (Failure (Suri.start_error_to_string error))

   let () = Runtime.run ~main ~args:Env.args ()
   ```

   Configure the server before starting it:

   ```ocaml
   match Suri.config ~port:8080 () with
   | Error errors ->
       Error (Failure (Suri.Config.errors_to_string errors))
   | Ok config ->
       Suri.start_link ~config app
   ```
*)
open Std

(**
   Server configuration including network settings, HTTP limits, and LiveView
   session security.
*)
module Config: sig
  (** Runtime environment. *)
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
  val default: t

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

(** Create server configuration with optional parameters. *)
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
  (Config.t, Config.error list) result

(** A middleware function: `Conn.t -> Conn.t`. *)
type middleware = Middleware.Pipeline.middleware
(** A handler is a middleware pipeline. *)
type handler = Middleware.Pipeline.t
type start_error =
  | InvalidConfig of Config.error list
  | InvalidAddress of Std.Net.Addr.error
  | BindFailed of Std.Net.TcpListener.error
  | InvalidAcceptors of int
  | InvalidBufferSize of int

val start_error_to_string: start_error -> string

(**
   Start a Suri web server with a middleware pipeline.

   Your application is simply a list of `Conn.t -> Conn.t` functions.
   Each middleware can transform the connection, set response data,
   or halt the pipeline.

   ```ocaml
   let app = [
     (fun conn -> Conn.respond conn ~status:Ok ~body:"Hello!");
   ]

   Suri.start_link app
   ```

   ```ocaml
   let app = [
     logger;
     auth;
     router;
   ]

   Suri.start_link app
   ```

   ```ocaml
   match Suri.config ~port:8080 () with
   | Error errors ->
       Error (Failure (Suri.Config.errors_to_string errors))
   | Ok config ->
       Suri.start_link ~config app
   ```
*)
val start_link: ?config:Config.t -> handler -> (Supervisor.Dynamic.t, start_error) result

(**
   Connection type and transformations.

   This is your primary API for handling requests in middleware.

   Core functions:
   - `respond ~status ~body` sets response data.
   - `with_header key value` adds a response header.
   - `with_body body` sets the response body.
   - `send` marks the connection as sent and halts the pipeline.
   - `uri`, `method_`, and `headers` expose request metadata.
   - `params` returns route parameters.

   See `Middleware.Conn` for the full API.
*)
module Conn = Middleware.Conn

(**
   HTTP Response builders.

   Most users should use `Conn.respond` in middleware, but `Response`
   is useful for building responses directly.

   See `Web_server.Response` for full documentation.
*)
module Response = Web_server.Response

(**
   HTTP Request accessors.

   Most users should use `Conn` methods in middleware, but `Request`
   is useful for extracting request data.

   See `Web_server.Request` for full documentation.
*)
module Request = Web_server.Request

(**
   Composable middleware framework.

   Includes:
   - `Middleware.Conn`: connection context.
   - `Middleware.Pipeline`: middleware composition.
   - `Middleware.Router`: pattern-based routing.

   See `Middleware` for complete documentation.
*)
module Middleware = Middleware

(**
   Type-safe HTML component system.

   Build UIs with React-style components that work with both
   static HTML generation and LiveView interactivity.

   Features:
   - 115+ HTML5 elements
   - 30+ attribute helpers
   - 15+ event handlers for LiveView
   - Conditional rendering helpers

   See `Component` for complete documentation.
*)
module Component = Component

(**
   Server-rendered components with live updates.

   Phoenix LiveView-style interactive UIs where events are
   sent to the server over WebSocket and DOM patches are
   sent back to the client.

   See `LiveView` for complete documentation.
*)
module LiveView = Liveview

(**
   Helpers for exercising real Suri applications and middleware in tests.

   See `Testing` for request builders, app runners, middleware helpers, and
   response expectations.
*)
module Testing = Testing
