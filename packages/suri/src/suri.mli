(** {1 Suri - High-Performance Web Framework for OCaml}

    Suri is a modern, actor-based web framework built on {!Std} and {!Miniriot}.
    It provides everything you need to build fast, fault-tolerant web applications
    with type safety and elegant concurrency.

    {2 Table of Contents}

    - {{!section-why}Why Suri?}
    - {{!section-quickstart}Quick Start}
    - {{!section-modules}Module Overview}
    - {{!section-examples}Examples}
    - {{!section-architecture}Architecture}

    {2:why Why Suri?}

    {b ✅ Actor-Based Concurrency}
    - Built on Miniriot's lightweight processes
    - Supervised connection pools with automatic restart
    - Handle thousands of concurrent connections efficiently

    {b ✅ Type-Safe Components}
    - React-style component system for building UIs
    - Write once, render as static HTML or interactive LiveView
    - No inline JavaScript required

    {b ✅ Composable Middleware}
    - Router with parameter extraction
    - Pipeline-based request processing
    - Easy to write custom middleware

    {b ✅ Production Ready}
    - HTTP/1.1 with keep-alive support
    - WebSocket support via Channel API
    - Fault tolerance through supervision trees

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
            receive_any ()  (* Keep alive *)
        | Error `Bind_error ->
            Error (Failure "Failed to bind")
    ]}

    {3 With Routing}

    {[
      open Std
      open Suri

      let routes =
        let open Middleware.Router in
        [
          get "/" (fun _conn _req ->
            WebServer.Response.ok ~body:"Home" ());
          
          get "/api/status" (fun _conn _req ->
            WebServer.Response.ok
              ~headers:(Http.Header.of_list [("Content-Type", "application/json")])
              ~body:{|{"status":"ok"}|}
              ());
          
          post "/api/echo" (fun _conn req ->
            let body = WebServer.Request.body req in
            WebServer.Response.ok ~body ());
        ]

      let handler =
        Middleware.Pipeline.create ()
        |> Middleware.Pipeline.plug (Middleware.Router.create routes)
        |> Middleware.Pipeline.to_handler

      let () = run_with @@ fun () ->
        let config = WebServer.Config.make () in
        match WebServer.start_link ~port:8080 ~config ~handler () with
        | Ok _supervisor ->
            Log.info "Server running with routes on http://0.0.0.0:8080";
            receive_any ()
        | Error `Bind_error ->
            Error (Failure "Failed to bind")
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

    All examples are available in [packages/suri/examples/] and documented in
    [packages/suri/EXAMPLES.md].

    {3 Available Examples}

    - [hello_world.ml] - Minimal HTTP server
    - [routing.ml] - Router with middleware pipeline
    - [json_api.ml] - RESTful JSON API with parameter extraction
    - [basic_component.ml] - Full-page component example with forms
    - [design_system.ml] - Reusable component library pattern
    - [liveview_migration.ml] - Static HTML → LiveView migration guide

    {b Run an example:}
    {[
      tusk run suri:hello_world
      tusk run suri:routing
      tusk run suri:basic_component
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

    - Read the full examples in [EXAMPLES.md]
    - Explore the {!Component} module for UI building
    - Check out {!Middleware.Router} for routing patterns
    - See {!WebServer.Response} for response helpers

    ---

    {1 API Reference} *)

module SocketPool = Socket_pool
(** {b TCP Connection Pool with Protocol Abstraction}

    Low-level module for building custom protocol servers. Most users should
    use {!WebServer} instead, which is built on top of SocketPool.

    {b Use SocketPool when:}
    - Building non-HTTP protocols (Redis, MQTT, etc.)
    - Need fine-grained control over connection lifecycle
    - Implementing custom protocol handlers

    {b Features:}
    - Supervised acceptor pool with configurable concurrency
    - Protocol switching support
    - Automatic connection cleanup
    - Backpressure handling

    See [examples/echo_server.ml] for a custom protocol example. *)

module WebServer = Web_server
(** {b HTTP/1.1 Web Server}

    High-level HTTP server built on {!SocketPool}. This is the main entry point
    for building web applications.

    {b Use WebServer for:}
    - REST APIs
    - Server-side rendered applications
    - Static file servers
    - Webhook receivers

    {b Features:}
    - HTTP/1.1 with keep-alive
    - Request parsing (headers, body, query params)
    - Response builders with status codes
    - Chunked transfer encoding support
    - Integration with {!Middleware} pipeline

    {b Quick Example:}
    {[
      let handler _conn req =
        let path = WebServer.Request.path req in
        match path with
        | "/" -> WebServer.Response.ok ~body:"Home" ()
        | _ -> WebServer.Response.not_found ~body:"404" ()

      let () = run_with @@ fun () ->
        let config = WebServer.Config.make () in
        WebServer.start_link ~port:8080 ~config ~handler ()
        |> Result.get_ok
        |> fun _ -> receive_any ()
    ]}

    See {!WebServer.Request} and {!WebServer.Response} for request/response APIs.
    See [examples/hello_world.ml] and [examples/routing.ml] for complete examples. *)

module Middleware = Middleware
(** {b Composable Middleware Framework}

    Pipeline-based middleware system for composing request/response transformations.

    {b Use Middleware for:}
    - Routing with parameter extraction
    - Logging and metrics
    - Authentication/authorization
    - Request transformation
    - Response compression

    {b Features:}
    - {!Middleware.Router} - Pattern-based routing with [`:param`] extraction
    - {!Middleware.Pipeline} - Compose middleware functions
    - Halt support - stop pipeline early
    - Conn abstraction - pass data between middleware

    {b Example:}
    {[
      let routes =
        let open Middleware.Router in
        [
          get "/" (fun _conn _req -> Response.ok ~body:"Home" ());
          get "/users/:id" (fun conn _req ->
            let id = Middleware.Conn.param conn "id" in
            Response.ok ~body:("User " ^ id) ());
        ]

      let handler =
        Middleware.Pipeline.create ()
        |> Middleware.Pipeline.plug (Middleware.Router.create routes)
        |> Middleware.Pipeline.to_handler
    ]}

    See {!Middleware.Router} for routing patterns.
    See [examples/routing.ml] and [examples/json_api.ml] for complete examples. *)

module Channel = Channel
(** {b WebSocket Communication Layer}

    Handler abstraction for building WebSocket servers and real-time features.

    {b Use Channel for:}
    - WebSocket servers
    - Real-time chat applications
    - Live dashboards
    - Push notifications
    - LiveView backend (coming soon)

    {b Features:}
    - WebSocket handshake handling
    - Message encoding/decoding
    - Connection lifecycle management
    - Integration with supervision tree

    {b Status:} WebSocket support is functional but API is still evolving.

    See [examples/websocket_example.ml] for usage patterns. *)

module Component = Component
(** {b Type-Safe HTML Component System}

    React-style component library for building UIs that work with both
    static HTML rendering and interactive LiveView applications.

    {b Why use Components?}

    ✅ {b Write Once, Render Anywhere}
    - Same components work for static HTML and LiveView
    - Preview components as static HTML during development
    - Add interactivity incrementally with event handlers

    ✅ {b Type Safety}
    - Catch HTML errors at compile time
    - No typos in class names or attributes
    - Refactor with confidence

    ✅ {b Composability}
    - Build reusable component libraries
    - Create design systems with consistent styling
    - Nest components naturally

    ✅ {b No JavaScript Required}
    - Event handlers run on the server (LiveView)
    - No client-side build step
    - No framework lock-in

    {b Quick Example:}
    {[
      open Suri.Component

      let card ~title ~content =
        div ~attrs:[class_ "card"] [
          h3 [text title];
          p [text content];
        ]

      let page =
        html [
          head [title_ [text "My App"]];
          body [
            card ~title:"Welcome" ~content:"Hello, Components!";
          ];
        ]

      let html_string = to_html page
    ]}

    {b Component Categories:}
    - 115+ HTML5 elements - complete coverage including semantic HTML5, forms, tables, multimedia, SVG, MathML
    - 30+ attribute helpers (class_, style, id, href, src, type_, etc.)
    - 15+ event handlers for LiveView (on_click, on_submit, on_input, etc.)
    - Conditional rendering (when_, unless, maybe)
    - Content helpers (text, int, float, fragment, empty)

    {b Examples:}
    - [examples/basic_component.ml] - Full-page component with forms
    - [examples/design_system.ml] - Reusable component library
    - [examples/liveview_migration.ml] - Static → LiveView migration

    See {!Component} module documentation for complete API reference. *)

module LiveView = Liveview
(** {b Server-Rendered Components with Live Updates}

    LiveView provides Phoenix LiveView-style interactive UIs where:
    - UI renders server-side with {!Component}
    - User events sent to server over WebSocket
    - Server updates state and re-renders
    - DOM patches sent back to client
    - No client-side JavaScript framework required

    {b Example:}
    {[
      module Counter = struct
        type state = { count: int }
        type msg = Increment | Decrement
        
        let init _conn = { count: 0 }
        let update msg state =
          match msg with
          | Increment -> { count = state.count + 1 }
          | Decrement -> { count = state.count - 1 }
        
        let render ~state () =
          Component.(
            div [
              h1 [text (Int.to_string state.count)];
              button ~attrs:[on_click (fun _ -> Decrement)] [text "-"];
              button ~attrs:[on_click (fun _ -> Increment)] [text "+"];
            ]
          )
      end
      
      let routes = [LiveView.route "/counter" (module Counter)]
    ]}

    See {!LiveView} module for full API and examples. *)
