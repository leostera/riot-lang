(**
   # HTTP Router

   Pattern-based HTTP router with parameter extraction and method matching.

   ## Quick Start

   ```ocaml
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
   ```

   ## Pattern Syntax

   ### Exact Match

   ```ocaml
     get "/" handler          (* Matches exactly "/" *)
     get "/users" handler     (* Matches exactly "/users" *)
   ```

   ### Parameter Capture

   ```ocaml
     get "/users/:id" handler
     (* Matches: /users/123, /users/alice *)
     (* Captures: id="123", id="alice" *)

     get "/posts/:post_id/comments/:id" handler
     (* Matches: /posts/42/comments/1 *)
     (* Captures: post_id="42", id="1" *)
   ```

   ### Accessing Parameters

   ```ocaml
     let user_handler conn req =
       let id = List.assoc_opt "id" (Conn.params conn)
         |> Option.unwrap_or ~default:"unknown" in
       Conn.respond conn ~status:Ok ~body:("User " ^ id) |> Conn.send
   ```

   ## HTTP Methods

   ### Standard Methods

   ```ocaml
     get "/resource" handler      (* GET *)
     post "/resource" handler     (* POST *)
     put "/resource/:id" handler  (* PUT *)
     patch "/resource/:id" handler (* PATCH *)
     delete "/resource/:id" handler (* DELETE *)
     head "/resource" handler     (* HEAD *)
   ```

   ## Route Grouping

   Group routes under a common prefix:

   ```ocaml
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
   ```

   ## 404 Handling

   Router automatically passes to next middleware for unmatched routes.
   Add a catch-all at the end of your pipeline:

   ```ocaml
     let app = [
       Middleware.router routes;
       (fun ~conn ~next:_ ->
         Conn.respond conn ~status:NotFound ~body:"404 Not Found" |> Conn.send);
     ]
   ```

   ## 405 Handling

   If a path matches one or more routes but the request method does not match
   any of them, the router returns `405 Method Not Allowed` and sets the
   `Allow` response header to the methods accepted by that path.

   ## Examples

   See `packages/suri/examples/`:
   - `routing.ml` - Basic routing example
   - `json_api.ml` - RESTful API with parameters

   ---

   # API Reference
*)

(**
   A route handler receives the connection and the original request.

   Route handlers are terminal - they should return a sent connection.
   Unlike middleware, handlers don't have access to `next` since routes
   are endpoints, not transformations in a pipeline.

   The connection provides convenient accessors for common request data,
   while the raw request is available for advanced use cases.

   Example:

   ```ocaml
     let home_handler conn req =
       Conn.respond conn ~status:Ok ~body:"Welcome!" |> Conn.send

     let user_handler conn req =
       let id = List.assoc_opt "id" (Conn.params conn) |> Option.unwrap_or ~default:"" in
       let body = Web_server.Request.body req in
       Conn.respond conn ~status:Ok ~body:("User " ^ id) |> Conn.send
   ```
*)
type handler = Conn.t -> Web_server.Request.t -> Conn.t

(** A single route definition with pattern, method, and handler. *)
type route

(** A router is a list of routes, matched in order. *)
type t = route list

(**
   Create a route that matches any HTTP method.

   Useful for WebSocket routes or handlers that need to accept multiple methods.

   ```ocaml
     any "/ws" websocket_handler
     any "/flexible" (fun conn req -> Conn.send conn)
   ```
*)
val any: string -> handler -> route

(**
   Create a GET route.

   ```ocaml
     get "/users" (fun conn req ->
       Conn.respond conn ~status:Ok ~body:"List of users" |> Conn.send)
   ```
*)
val get: string -> handler -> route

(**
   Create a POST route.

   ```ocaml
     post "/users" (fun conn req ->
       let body = Web_server.Request.body req in
       Conn.respond conn ~status:Created ~body:("Created: " ^ body) |> Conn.send)
   ```
*)
val post: string -> handler -> route

(**
   Create a PUT route.

   ```ocaml
     put "/users/:id" (fun conn req ->
       let id = List.assoc_opt "id" (Conn.params conn) |> Option.unwrap_or ~default:"" in
       let body = Web_server.Request.body req in
       Conn.respond conn ~status:Ok ~body:("Updated user " ^ id) |> Conn.send)
   ```
*)
val put: string -> handler -> route

(**
   Create a PATCH route.

   ```ocaml
     patch "/users/:id" (fun conn req ->
       let id = List.assoc_opt "id" (Conn.params conn) |> Option.unwrap_or ~default:"" in
       Conn.respond conn ~status:Ok ~body:("Patched user " ^ id) |> Conn.send)
   ```
*)
val patch: string -> handler -> route

(**
   Create a DELETE route.

   ```ocaml
     delete "/users/:id" (fun conn req ->
       let id = List.assoc_opt "id" (Conn.params conn) |> Option.unwrap_or ~default:"" in
       Conn.respond conn ~status:Ok ~body:("Deleted user " ^ id) |> Conn.send)
   ```
*)
val delete: string -> handler -> route

(**
   Create a HEAD route (like GET but no response body).

   ```ocaml
     head "/resource" (fun conn req ->
       Conn.respond conn ~status:Ok |> Conn.send)
   ```
*)
val head: string -> handler -> route

(**
   Group routes under a common path prefix.

   ```ocaml
     scope "/api" [
       get "/health" health_handler;
       scope "/v1" [
         get "/users" list_users;
       ];
     ]
     (* Creates routes: /api/health, /api/v1/users *)
   ```
*)
val scope: string -> route list -> route

(**
   Forward every request under a path prefix to another route tree.

   Unlike `scope`, `forward` treats the mount path itself as the route match:
   any method for the exact prefix or a descendant path is handled by the
   forwarded route tree. Prefix matching is path-boundary aware, so forwarding
   `"/mailer"` matches `"/mailer"` and `"/mailer/messages"`, but not
   `"/maileroo"`.

   ```ocaml
     scope "/__suri" [
       forward "/mailer" mailer_routes;
       forward "/jobs" jobs_routes;
     ]
   ```
*)
val forward : string -> route list -> route

(**
   Create a WebSocket route that upgrades HTTP connections to WebSocket.

   This route handles both the initial HTTP request (for non-WebSocket clients)
   and WebSocket upgrade requests.

   ```ocaml
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
   ```

   The handler module must implement `Channel.Handler.Intf`.
*)
val websocket:
  string ->
  (module Channel.Handler.Intf with type args = 'a and type state = 's) ->
  'a ->
  route

val match_path: string -> string -> (string * string) list option

(**
   Convert a list of routes into middleware.

   This is the main function to use with `Pipeline`.

   The router middleware tries to match the request path and method against
   all routes in order. When a match is found, the route's handler is called.
   If the path matches but the method does not, the router returns
   `405 Method Not Allowed`. If no route path matches, the `next` middleware
   in the pipeline is called.

   ```ocaml
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
   ```
*)
val middleware: t -> Pipeline.middleware
