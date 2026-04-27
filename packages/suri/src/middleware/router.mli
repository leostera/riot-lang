(**
   {1 HTTP Router}

   Pattern-based HTTP router with parameter extraction and method matching.

   {2 Quick Start}

   {[
     open Suri.Middleware

     let routes =
       let open Router in
       [
         get "/" (fun conn req ->
           Conn.respond conn ~status:Ok ~body:"Home" |> Conn.send);

         get "/users/:id" (fun conn req ->
           let id = List.assoc_opt "id" (Conn.params conn) |> Option.unwrap_or ~default:"" in
           Conn.respond conn ~status:Ok ~body:("User " ^ id) |> Conn.send);

         post "/users" (fun conn req ->
           let body = Web_server.Request.body req in
           Conn.respond conn ~status:Created ~body:("Created: " ^ body) |> Conn.send);
       ]

     (* Use router in middleware pipeline *)
     let app = [ Router.middleware routes ]
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
     let user_handler conn req =
       let id = List.assoc_opt "id" (Conn.params conn)
         |> Option.unwrap_or ~default:"unknown" in
       Conn.respond conn ~status:Ok ~body:("User " ^ id) |> Conn.send
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

   Router automatically passes to next middleware for unmatched routes.
   Add a catch-all at the end of your pipeline:
   {[
     let app = [
       Middleware.router routes;
       (fun ~conn ~next:_ ->
         Conn.respond conn ~status:NotFound ~body:"404 Not Found" |> Conn.send);
     ]
   ]}

   {2 405 Handling}

   If a path matches one or more routes but the request method does not match
   any of them, the router returns [405 Method Not Allowed] and sets the
   [Allow] response header to the methods accepted by that path.

   {2 Examples}

   See [packages/suri/examples/]:
   - [routing.ml] - Basic routing example
   - [json_api.ml] - RESTful API with parameters

   ---

   {1 API Reference}
*)

(**
   A route handler receives the connection and the original request.

   Route handlers are terminal - they should return a sent connection.
   Unlike middleware, handlers don't have access to [next] since routes
   are endpoints, not transformations in a pipeline.

   The connection provides convenient accessors for common request data,
   while the raw request is available for advanced use cases.

   Example:
   {[
     let home_handler conn req =
       Conn.respond conn ~status:Ok ~body:"Welcome!" |> Conn.send

     let user_handler conn req =
       let id = List.assoc_opt "id" (Conn.params conn) |> Option.unwrap_or ~default:"" in
       let body = Web_server.Request.body req in
       Conn.respond conn ~status:Ok ~body:("User " ^ id) |> Conn.send
   ]}
*)

(** A single route definition with pattern, method, and handler. *)
type handler = Conn.t -> Web_server.Request.t -> Conn.t
(** A router is a list of routes, matched in order. *)
type route
(**
   Create a route that matches any HTTP method.

   Useful for WebSocket routes or handlers that need to accept multiple methods.

   {[
     any "/ws" websocket_handler
     any "/flexible" (fun conn req -> Conn.send conn)
   ]}
*)
type t = route list
val any: string -> handler -> route

(**
   Create a GET route.

   {[
     get "/users" (fun conn req ->
       Conn.respond conn ~status:Ok ~body:"List of users" |> Conn.send)
   ]}
*)
val get: string -> handler -> route

(**
   Create a POST route.

   {[
     post "/users" (fun conn req ->
       let body = Web_server.Request.body req in
       Conn.respond conn ~status:Created ~body:("Created: " ^ body) |> Conn.send)
   ]}
*)
val post: string -> handler -> route

(**
   Create a PUT route.

   {[
     put "/users/:id" (fun conn req ->
       let id = List.assoc_opt "id" (Conn.params conn) |> Option.unwrap_or ~default:"" in
       let body = Web_server.Request.body req in
       Conn.respond conn ~status:Ok ~body:("Updated user " ^ id) |> Conn.send)
   ]}
*)
val put: string -> handler -> route

(**
   Create a PATCH route.

   {[
     patch "/users/:id" (fun conn req ->
       let id = List.assoc_opt "id" (Conn.params conn) |> Option.unwrap_or ~default:"" in
       Conn.respond conn ~status:Ok ~body:("Patched user " ^ id) |> Conn.send)
   ]}
*)
val patch: string -> handler -> route

(**
   Create a DELETE route.

   {[
     delete "/users/:id" (fun conn req ->
       let id = List.assoc_opt "id" (Conn.params conn) |> Option.unwrap_or ~default:"" in
       Conn.respond conn ~status:Ok ~body:("Deleted user " ^ id) |> Conn.send)
   ]}
*)
val delete: string -> handler -> route

(**
   Create a HEAD route (like GET but no response body).

   {[
     head "/resource" (fun conn req ->
       Conn.respond conn ~status:Ok |> Conn.send)
   ]}
*)
val head: string -> handler -> route

(**
   Group routes under a common path prefix.

   {[
     scope "/api" [
       get "/health" health_handler;
       scope "/v1" [
         get "/users" list_users;
       ];
     ]
     (* Creates routes: /api/health, /api/v1/users *)
   ]}
*)
val scope: string -> route list -> route

(**
   Create a WebSocket route that upgrades HTTP connections to WebSocket.

   This route handles both the initial HTTP request (for non-WebSocket clients)
   and WebSocket upgrade requests.

   {[
     module EchoHandler = struct
       type args = unit
       type state = unit

       let init () = Channel.Handler.Continue ()

       let handle_frame frame _conn state =
         match frame with
         | { Http.Ws.Frame.opcode = Text; payload; _ } ->
             let response = Http.Ws.Frame.text payload in
             Channel.Handler.Push ([response], state)
         | _ -> Channel.Handler.Continue state

       let handle_message _msg state = Channel.Handler.Continue state
     end

     let routes = [
       websocket "/ws/echo" (module EchoHandler) ();
     ]
   ]}

   The handler module must implement {!Channel.Handler.Intf}.
*)
val websocket:
  string ->
  (module Channel.Handler.Intf with type args = 'a and type state = 's) ->
  'a ->
  route

val match_path: string -> string -> (string * string) list option

(**
   Convert a list of routes into middleware.

   This is the main function to use with {!Pipeline}.

   The router middleware tries to match the request path and method against
   all routes in order. When a match is found, the route's handler is called.
   If the path matches but the method does not, the router returns
   [405 Method Not Allowed]. If no route path matches, the [next] middleware
   in the pipeline is called.

   {[
     let routes = [
       get "/" (fun conn ->
         Conn.respond conn ~status:Ok ~body:"Home" |> Conn.send);
       get "/about" (fun conn ->
         Conn.respond conn ~status:Ok ~body:"About" |> Conn.send);
     ]

     (* Use in middleware pipeline *)
     let app = Middleware.[
       logger;
       Router.middleware routes;
     ]
   ]}
*)
val middleware: t -> Pipeline.middleware
