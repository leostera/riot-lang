(** {1 Middleware Framework}

    Composable middleware system for HTTP request/response processing.
    Build request pipelines that transform, route, log, and respond to HTTP requests.

    {2 Table of Contents}

    - {{!section-why}Why Middleware?}
    - {{!section-quickstart}Quick Start}
    - {{!section-concepts}Core Concepts}
    - {{!section-modules}Modules}
    - {{!section-examples}Examples}

    {2:why Why Middleware?}

    {b ✅ Composability}
    - Chain transformations with [|>] operator
    - Reusable middleware components
    - Easy to test in isolation

    {b ✅ Request Pipeline}
    - Logging, authentication, routing in one flow
    - Early termination with [Conn.halt]
    - Pass data between middleware via [Conn.assign]

    {b ✅ Type-Safe Routing}
    - Pattern matching with parameter extraction
    - [/users/:id] captures [id] parameter
    - Method-specific routes (GET, POST, etc.)

    {2:quickstart Quick Start}

    {3 Simple Router}

    {[
      open Std
      open Suri

      let routes =
        let open Middleware.Router in
        [
          get "/" (fun _conn _req ->
            WebServer.Response.ok ~body:"Home" ());
          
          get "/users/:id" (fun conn _req ->
            let id = Middleware.Conn.param conn "id" in
            WebServer.Response.ok ~body:("User " ^ id) ());
          
          post "/api/data" (fun _conn req ->
            let body = WebServer.Request.body req in
            WebServer.Response.ok ~body ());
        ]

      let handler =
        Middleware.Pipeline.create ()
        |> Middleware.Pipeline.plug (Middleware.Router.create routes)
        |> Middleware.Pipeline.to_handler
    ]}

    {3 Custom Middleware}

    {[
      let logger_middleware next conn =
        let start_time = Unix.gettimeofday () in
        let conn = next conn in
        let duration = Unix.gettimeofday () -. start_time in
        Log.info "Request took %.2fms" (duration *. 1000.0);
        conn

      let handler =
        Middleware.Pipeline.create ()
        |> Middleware.Pipeline.plug logger_middleware
        |> Middleware.Pipeline.plug (Middleware.Router.create routes)
        |> Middleware.Pipeline.to_handler
    ]}

    {2:concepts Core Concepts}

    {3 Connection (Conn)}

    A {!Conn.t} represents the connection state flowing through the pipeline:
    - Contains the original request
    - Accumulates response data (status, headers, body)
    - Carries middleware-specific state via [assign]
    - Can be halted to stop pipeline execution

    {3 Pipeline}

    A {!Pipeline.t} is a chain of middleware functions that transform connections:
    {[
      type middleware = (Conn.t -> Conn.t) -> Conn.t -> Conn.t
    ]}

    Each middleware can:
    - Inspect/modify the request
    - Call the next middleware in the chain
    - Set response data
    - Halt the pipeline early

    {3 Router}

    The {!Router} matches request paths to handler functions:
    - Pattern syntax: [/users/:id/posts/:post_id]
    - Extracts parameters and stores them in [Conn]
    - Method-specific routing (GET, POST, PUT, DELETE)
    - 404 fallback for unmatched routes

    {2:modules Modules}

    - {!Conn} - Connection context with request, response, and state
    - {!Pipeline} - Compose and execute middleware chains
    - {!Router} - Pattern-based routing with parameter extraction

    {2:examples Examples}

    See [packages/suri/examples/]:
    - [routing.ml] - Router with middleware pipeline
    - [json_api.ml] - RESTful API with parameter extraction
    - [middleware_example.ml] - Custom middleware patterns

    Run examples:
    {[
      tusk run suri:routing
      tusk run suri:json_api
    ]}

    ---

    {1 API Reference} *)

module Conn = Conn
(** {b Connection Context}

    Represents the connection state flowing through middleware.
    
    Contains:
    - Original HTTP request
    - Response data (status, headers, body)
    - Route parameters (from Router)
    - Custom state (via [assign])
    - Halt flag (stop pipeline)

    See {!Conn} for full API. *)

module Pipeline = Pipeline
(** {b Middleware Pipeline}

    Compose and execute middleware functions in sequence.

    {b Key functions:}
    - [create ()] - New empty pipeline
    - [plug middleware] - Add middleware to pipeline
    - [to_handler] - Convert pipeline to WebServer handler
    - [run conn] - Execute pipeline on connection

    See {!Pipeline} for full API. *)

module Router = Router
(** {b HTTP Router}

    Pattern-based routing with parameter extraction.

    {b Route patterns:}
    - ["/"] - Exact match
    - ["/users/:id"] - Captures [id] parameter
    - ["/posts/:id/comments/:cid"] - Multiple parameters

    {b Route methods:}
    - [get], [post], [put], [delete], [patch]
    - [any] - Matches all methods

    {b Example:}
    {[
      let routes = [
        Router.get "/" home_handler;
        Router.get "/users/:id" user_handler;
        Router.post "/api/data" create_handler;
      ]

      let router = Router.create routes
    ]}

    See {!Router} for full API. *)
