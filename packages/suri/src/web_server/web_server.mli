(**
   {1 WebServer - HTTP/1.1 Server}

   Experimental HTTP/1.1 server foundation with connection pooling, keep-alive
   support, and automatic error recovery through supervision.

   {2 Table of Contents}

   - {{!section-why}Why WebServer?}
   - {{!section-quickstart}Quick Start}
   - {{!section-features}Features}
   - {{!section-modules}Modules}
   - {{!section-examples}Examples}

   {2:why Why WebServer?}

   {b Current Status}
   - HTTP/1.1 foundation with keep-alive connections
   - Supervised connection pool for acceptors
   - Request timeouts, connection limits, graceful shutdown, and full protocol
     hardening are still in progress

   {b ✅ Simple API}
   - Handler function: [Request.t -> Response.t]
   - Response builders: [ok], [not_found], [redirect], etc.
   - Request accessors: [path], [method_], [headers], [body]

   {b ✅ Flexible}
   - Works standalone or with {!Middleware}
   - Configurable buffer sizes and timeouts
   - Custom acceptor pool size
   - Integration with supervision trees

   {2:quickstart Quick Start}

   {3 Hello World}

   {[
     open Std
     open Suri

     let handler _conn _req =
       WebServer.Response.ok ~body:"Hello, World!" ()

     let () = run_with @@ fun () ->
       let config = WebServer.Config.make () in
       match WebServer.start_link ~port:8080 ~config ~handler () with
       | Ok _supervisor ->
           Log.info "Server running on http://0.0.0.0:8080";
           receive_any ()
       | Error error ->
           Log.error (WebServer.start_error_to_string error);
           Ok ()
   ]}

   {3 With Request Inspection}

   {[
     let handler _conn req =
       let open WebServer in
       let path = Request.path req in
       let method_ = Request.method_ req in

       Log.info "%s %s" (Http.Method.to_string method_) path;

       match (method_, path) with
       | (GET, "/") ->
           Response.ok ~body:"Home" ()
       | (GET, "/about") ->
           Response.ok ~body:"About Us" ()
       | (POST, "/api/data") ->
           let body = Request.body req in
           Response.ok ~body:("Received: " ^ body) ()
       | _ ->
           Response.not_found ~body:"404 - Not Found" ()
   ]}

   {3 With JSON Response}

   {[
     let handler _conn _req =
       let json = Data.Json.obj [
         ("status", Data.Json.string "ok");
         ("message", Data.Json.string "Hello from Suri");
       ] in
       WebServer.Response.ok
         ~headers:(Http.Header.from_list [("Content-Type", "application/json")])
         ~body:(Data.Json.to_string json)
         ()
   ]}

   {2:features Features}

   {3 HTTP/1.1 Support}
   - Keep-alive connections (persistent connections)
   - Chunked transfer encoding
   - Request header parsing
   - Query parameter extraction
   - POST body handling

   {3 Supervision & Fault Tolerance}
   - Supervised acceptor pool
   - Automatic process restart on crash
   - Connection cleanup on error
   - Graceful degradation

   {3 Performance}
   - Configurable acceptor pool size (default: 100)
   - Adjustable buffer sizes
   - Connection pooling and reuse
   - Minimal memory allocations

   {2:modules Modules}

   - {!Config} - Server configuration (buffer size, timeouts, etc.)
   - {!Request} - HTTP request inspection and parsing
   - {!Response} - HTTP response builders (ok, redirect, error, etc.)
   - {!Http1} - Low-level HTTP/1.1 protocol handler

   {2:examples Examples}

   See [packages/suri/examples/]:
   - [hello_world.ml] - Minimal HTTP server
   - [routing.ml] - Router with middleware pipeline
   - [json_api.ml] - RESTful JSON API

   Run examples:
   {[
     riot run suri:hello_world
     riot run suri:routing
     riot run suri:json_api
   ]}

   {2 Configuration}

   {3 Tune Acceptors}

   Increase for high concurrency:
   {[
     let config = Config.make ~acceptors:200 ()
     WebServer.start_link ~port:8080 ~config ~handler ()
   ]}

   {3 Adjust Buffer Size}

   Larger buffers for big requests:
   {[
     let config = Config.make ~buffer_size:8192 ()
   ]}

   {3 Monitor Health}

   {[
     match WebServer.start_link ~port:8080 ~config ~handler () with
     | Ok supervisor ->
         let count = Supervisor.Dynamic.count_children supervisor in
         Log.info "Active acceptors: %d" count.active;
         receive_any ()
     | Error error ->
         Log.error (WebServer.start_error_to_string error);
         Ok ()
   ]}

   ---

   {1 API Reference}
*)

module Config = Config

(**
   {b Server Configuration}

   Configure server behavior, buffer sizes, and connection limits.

   {b Example:}
   {[
     let config = Config.make
       ~buffer_size:4096
       ~max_request_size:1048576  (* 1MB *)
       ()
   ]}

   See {!Config} for all options.
*)
module Request = Request

(**
   {b HTTP Request}

   Inspect incoming HTTP requests.

   {b Common operations:}
   - [Request.path req] - Get request path
   - [Request.method_ req] - Get HTTP method
   - [Request.headers req] - Get all headers
   - [Request.body req] - Get request body
   - [Request.query_param req "name"] - Get query parameter

   {b Example:}
   {[
     let handler _conn req =
       let path = Request.path req in
       let user_agent = Request.header req "User-Agent" in
       Log.info "Request to %s from %s" path user_agent;
       Response.ok ~body:"OK" ()
   ]}

   See {!Request} for full API.
*)
module Response = Response

(**
   {b HTTP Response}

   Build HTTP responses with status codes, headers, and body.

   {b Response builders:}
   - [ok ~body] - 200 OK
   - [created ~body] - 201 Created
   - [redirect ~location] - 302 Redirect
   - [bad_request ~body] - 400 Bad Request
   - [unauthorized ~body] - 401 Unauthorized
   - [not_found ~body] - 404 Not Found
   - [internal_server_error ~body] - 500 Internal Server Error

   {b Example:}
   {[
     let handler _conn req =
       match Request.path req with
       | "/redirect" ->
           Handler.close (Response.redirect ~location:"/home" ())
       | "/json" ->
           Handler.close (Response.ok
             ~headers:(Http.Header.from_list [("Content-Type", "application/json")])
             ~body:{|{"status":"ok"}|}
             ())
       | _ ->
           Handler.close (Response.not_found ~body:"Page not found" ())
   ]}

   See {!Response} for full API.
*)
module Handler = Http_handler

(**
   {b HTTP Handler}

   Handler functions that can return either HTTP responses or protocol upgrades (WebSocket).

   See {!Handler} for full API.
*)
module Http1 = Http1_handler

(**
   {b HTTP/1.1 Protocol Handler}

   Low-level HTTP/1.1 protocol implementation.

   Most users don't need to use this directly - use {!start_link} instead.

   See {!Http1} for internals.
*)
module Http2 = Http2_handler

(** HTTP/2 protocol handler *)
module ProtocolDetector = Protocol_detector

(** Auto-detect HTTP/1.1 vs HTTP/2 and switch handlers *)
type start_error =
  | InvalidAddress of Std.Net.Addr.error
  | BindFailed of Std.Net.TcpListener.error
  | InvalidAcceptors of int
  | InvalidBufferSize of int

val start_error_to_string: start_error -> string

val start_link:
  ?host:string ->
  port:int ->
  ?acceptors:int ->
  config:Config.t ->
  handler:Handler.t ->
  unit ->
  (Std.Supervisor.Dynamic.t, start_error) Std.result

(**
   Start a supervised HTTP/1.1 server.

   This is the main entry point for starting a Suri web server.
   It creates a supervised socket pool that handles HTTP/1.1 connections
   with automatic acceptor restart on failure.

   @param host Host to bind to (default: "0.0.0.0" for all interfaces)
   @param port Port to listen on
   @param acceptors Number of concurrent acceptor processes (default: 100)
   @param config Server configuration (see {!Config})
   @param handler Your HTTP request handler function
   @return [Ok supervisor] with the supervisor PID, or [Error _] if startup fails

   The handler function receives a connection and request, and should return
   either a normal HTTP response or a protocol upgrade (WebSocket). Example:

   ```ocaml
   let handler _conn req =
     match Request.path req with
     | "/" -> Handler.close (Response.ok ~body:"Home" ())
     | "/about" -> Handler.close (Response.ok ~body:"About" ())
     | _ -> Handler.close (Response.not_found ~body:"Not Found" ())
   ```

   For WebSocket upgrades, return {!Handler.websocket}:
   ```ocaml
   let handler _conn req =
     match Request.path req with
     | "/ws/echo" ->
         let opts = Channel.Handler.{ do_upgrade = true } in
         let ws_handler = Channel.Handler.make (module EchoHandler) () in
         Handler.websocket opts ws_handler
     | _ -> Handler.close (Response.not_found ())
   ```

   The supervisor manages a pool of acceptor processes. If an acceptor crashes,
   it will be automatically restarted. The supervisor itself can be linked to
   your application's supervision tree.
*)
