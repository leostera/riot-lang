(** {1 HTTP Router}

    Pattern-based HTTP router with parameter extraction and method matching.

    {2 Quick Start}

    {[
      open Suri.Middleware

      let routes =
        let open Router in
        [
          get "/" (fun _conn _req ->
            WebServer.Response.ok ~body:"Home" ());
          
          get "/users/:id" (fun conn _req ->
            let id = Conn.param conn "id" in
            WebServer.Response.ok ~body:("User " ^ id) ());
          
          post "/users" (fun _conn req ->
            let body = WebServer.Request.body req in
            WebServer.Response.created ~body:("Created: " ^ body) ());
        ]

      (* Middleware is just a list! *)
      let app = [ Router.middleware routes ]

      let handler socket_conn req =
        let conn = Conn.make socket_conn req in
        let conn = Pipeline.run conn app in
        Conn.to_response conn
    ]}

    {2 Pattern Syntax}

    {3 Exact Match}
    {[
      get "/" handler          (* Matches exactly "/" *)
      get "/users" handler     (* Matches exactly "/users" *)
    ]}

    {3 Parameter Capture}
    {[
      get "/users/:id" handler
      (* Matches: /users/123, /users/alice *)
      (* Captures: id="123", id="alice" *)

      get "/posts/:post_id/comments/:id" handler
      (* Matches: /posts/42/comments/1 *)
      (* Captures: post_id="42", id="1" *)
    ]}

    {3 Accessing Parameters}
    {[
      let user_handler conn _req =
        let id = Conn.param conn "id" in
        WebServer.Response.ok ~body:("User " ^ id) ()
    ]}

    {2 HTTP Methods}

    {3 Standard Methods}
    {[
      get "/resource" handler      (* GET *)
      post "/resource" handler     (* POST *)
      put "/resource/:id" handler  (* PUT *)
      patch "/resource/:id" handler (* PATCH *)
      delete "/resource/:id" handler (* DELETE *)
      head "/resource" handler     (* HEAD *)
    ]}

    {2 Route Grouping}

    Group routes under a common prefix:
    {[
      let routes = [
        get "/" home_handler;
        
        scope "/api" [
          get "/health" health_handler;
          get "/version" version_handler;
          
          scope "/v1" [
            get "/users" list_users;
            get "/users/:id" get_user;
          ];
        ];
      ]
      (* Results in routes:
         /
         /api/health
         /api/version
         /api/v1/users
         /api/v1/users/:id *)
    ]}

    {2 404 Handling}

    Router automatically handles unmatched routes with a 404 response.
    You can customize this by adding a catch-all route:
    {[
      let routes = [
        get "/" home_handler;
        get "/about" about_handler;
        (* ... other routes ... *)
      ]
      (* Unmatched routes automatically return 404 *)
    ]}

    {2 Examples}

    See [packages/suri/examples/]:
    - [routing.ml] - Basic routing example
    - [json_api.ml] - RESTful API with parameters

    ---

    {1 API Reference} *)

type route
(** A single route definition with pattern, method, and handler. *)

type t = route list
(** A router is a list of routes, matched in order. *)

val any : string -> Pipeline.middleware -> route
(** Create a route that matches any HTTP method.
    
    Useful for WebSocket routes or handlers that need to accept multiple methods.
    
    {[
      any "/ws" websocket_handler
      any "/flexible" (fun conn -> ...)
    ]} *)

val get : string -> Pipeline.middleware -> route
(** Create a GET route.

    {[
      get "/users" (fun _conn _req ->
        Response.ok ~body:"List of users" ())
    ]} *)

val post : string -> Pipeline.middleware -> route
(** Create a POST route.

    {[
      post "/users" (fun _conn req ->
        let body = Request.body req in
        Response.created ~body:("Created: " ^ body) ())
    ]} *)

val put : string -> Pipeline.middleware -> route
(** Create a PUT route.

    {[
      put "/users/:id" (fun conn req ->
        let id = Conn.param conn "id" in
        let body = Request.body req in
        Response.ok ~body:("Updated user " ^ id) ())
    ]} *)

val patch : string -> Pipeline.middleware -> route
(** Create a PATCH route.

    {[
      patch "/users/:id" (fun conn req ->
        let id = Conn.param conn "id" in
        Response.ok ~body:("Patched user " ^ id) ())
    ]} *)

val delete : string -> Pipeline.middleware -> route
(** Create a DELETE route.

    {[
      delete "/users/:id" (fun conn _req ->
        let id = Conn.param conn "id" in
        Response.ok ~body:("Deleted user " ^ id) ())
    ]} *)

val head : string -> Pipeline.middleware -> route
(** Create a HEAD route (like GET but no response body).

    {[
      head "/resource" (fun _conn _req ->
        Response.ok ())
    ]} *)

val scope : string -> route list -> route
(** Group routes under a common path prefix.

    {[
      scope "/api" [
        get "/health" health_handler;
        scope "/v1" [
          get "/users" list_users;
        ];
      ]
      (* Creates routes: /api/health, /api/v1/users *)
    ]} *)

val websocket : 
  string -> 
  (module Channel.Handler.Intf with type args = 'a and type state = 's) -> 
  'a -> 
  route
(** Create a WebSocket route that upgrades HTTP connections to WebSocket.
    
    This route handles both the initial HTTP request (for non-WebSocket clients)
    and WebSocket upgrade requests.
    
    {[
      module EchoHandler = struct
        type args = unit
        type state = unit
        
        let init () = `ok ()
        
        let handle_frame frame _conn state =
          match frame with
          | { Http.Ws.Frame.opcode = Text; payload; _ } ->
              let response = Http.Ws.Frame.text payload in
              `push ([response], state)
          | _ -> `ok state
        
        let handle_message _msg state = `ok state
      end
      
      let routes = [
        websocket "/ws/echo" (module EchoHandler) ();
      ]
    ]}
    
    The handler module must implement {!Channel.Handler.Intf}. *)

val middleware : t -> Pipeline.middleware
(** Convert a list of routes into middleware.

    This is the main function to use with {!Pipeline}.

    {[
      let routes = [
        get "/" home_handler;
        get "/about" about_handler;
      ]

      (* Middleware is just a list! *)
      let app = [ Router.middleware routes ]

      let handler socket_conn req =
        let conn = Conn.make socket_conn req in
        Pipeline.run conn app
    ]} *)
