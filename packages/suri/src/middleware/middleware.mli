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

    {3 Simple Middleware Pipeline}

    Middleware is just a list of [Conn.t -> Conn.t] functions!

    {[
      open Std
      open Suri

      (* Define your middleware functions *)
      let logger conn =
        let uri = Conn.uri conn in
        Log.info ("Request: " ^ uri);
        conn

      (* Build routes *)
      let routes = Middleware.Router.[
        get "/" (fun conn -> Conn.respond conn ~status:Ok ~body:"Home");
        get "/about" (fun conn -> Conn.respond conn ~status:Ok ~body:"About");
      ]

      (* Pipeline is just a list! *)
      let app = [
        logger;
        Middleware.Router.middleware routes;
      ]

      (* Run the pipeline on each request *)
      let handler socket_conn req =
        let conn = Middleware.Conn.make socket_conn req in
        let conn = Middleware.Pipeline.run conn app in
        let response = Middleware.Conn.to_response conn in
        close response
    ]}

    {3 Custom Middleware}

    Write your own [Conn.t -> Conn.t] functions:

    {[
      (* Add a header to all responses *)
      let add_header conn =
        Conn.with_header conn "X-Powered-By" "Suri"

      (* Authenticate requests *)
      let auth conn =
        match Conn.get_header conn "Authorization" with
        | Some token when token = "secret" -> conn
        | _ -> Conn.halt (Conn.respond conn ~status:Unauthorized ~body:"Unauthorized")

      (* Compose them in a list *)
      let app = [
        logger;
        add_header;
        auth;
        router;
      ]
    ]}

    {2:concepts Core Concepts}

    {3 Connection (Conn)}

    A {!Conn.t} represents the connection state flowing through the pipeline:
    - Contains the original request
    - Accumulates response data (status, headers, body)
    - Carries middleware-specific state via [assign]
    - Can be halted to stop pipeline execution

    {3 Pipeline}

    A {!Pipeline.t} is simply a list of middleware functions:
    {[
      type middleware = Conn.t -> Conn.t
      type t = middleware list
    ]}

    Each middleware can:
    - Inspect/modify the connection
    - Transform the request/response
    - Set response data
    - Halt the pipeline early (stops execution)

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

    A pipeline is just a list of middleware functions.

    {b Type:}
    {[
      type middleware = Conn.t -> Conn.t
      type t = middleware list
    ]}

    {b Usage:}
    {[
      let app = [ logger; router; not_found ] in
      let conn = Pipeline.run conn app
    ]}

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

(** {2 Convenience Functions} *)

val router : Router.route list -> Pipeline.middleware
(** Create router middleware from a list of routes.
    
    This is a convenience alias for [Router.middleware routes].
    Makes middleware pipelines more readable:
    
    {[
      let app = [
        logger;
        router [
          Router.get "/" home;
          Router.get "/about" about;
        ];
      ]
    ]}
    
    Instead of:
    {[
      let app = [
        logger;
        Router.middleware [
          Router.get "/" home;
          Router.get "/about" about;
        ];
      ]
    ]} *)
