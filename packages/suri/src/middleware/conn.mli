(** HTTP request and response context flowing through middleware pipelines. *)
open Std

(** Peer connection information. *)
type peer = { ip: string; port: int }
(** Connection context. *)
type t

(** Create a new connection from a socket connection and parsed request. *)
val make: Socket_pool.Connection.t -> Web_server.Request.t -> t

(**
   Create a connection from an already parsed request without a live socket.
   This is useful for adapters and test harnesses that execute middleware
   directly.
*)
val from_request:
  ?peer:peer ->
  ?params:(string * string) list ->
  ?body_params:(string * string) list ->
  Web_server.Request.t ->
  t

(**
   Get the original HTTP request.

   Most handlers should use convenience accessors like `method_`, `uri`, `body`,
   etc. The raw request is available for advanced use cases.
*)
val request: t -> Web_server.Request.t

(** Get the HTTP method. *)
val method_: t -> Net.Http.Method.t

(** Get the request URI. *)
val uri: t -> string

(** Get the request path without the query string. *)
val path: t -> string

(** Parse an application/x-www-form-urlencoded query string. *)
val parse_query_params: string -> (string * string) list

(** Get request headers. *)
val headers: t -> Net.Http.Header.t

(** Set a request header for downstream middleware and handlers. *)
val with_request_header: string -> string -> t -> t

(** Get the request body. *)
val body: t -> string

(** Get path or query parameters. *)
val params: t -> (string * string) list

(**
   Get query parameters from the URL.

   Parses the query string from the request URI and returns parameter pairs.

   ```ocaml
   (* For URI: /path?foo=bar&baz=qux *)
   let params = Conn.query_params conn
   (* params = [("foo", "bar"); ("baz", "qux")] *)
   ```

   Note: URL-encoded values are automatically decoded.
*)
val query_params: t -> (string * string) list

(** Get parsed body parameters set by body parser middleware. *)
val body_params: t -> (string * string) list

(** Get peer connection information. *)
val peer: t -> peer

(**
   Get response headers that have been set so far.
   Useful for reading headers set by upstream middleware.
*)
val resp_headers: t -> (string * string) list

(** Set response status. *)
val with_status: Net.Http.Status.t -> t -> t

(** Set response body. *)
val with_body: string -> t -> t

(** Add a response header. *)
val with_header: string -> string -> t -> t

(** Set a response header, replacing any previous values with the same case-insensitive name. *)
val set_header: string -> string -> t -> t

(** Set status and optionally body. *)
val respond: status:Net.Http.Status.t -> ?body:string -> t -> t

(** Mark the connection as ready to send a response. *)
val send: t -> t

(** Check if the response has been sent. *)
val sent: t -> bool

(**
   Render an HTML component as response with proper content-type.

   This is a convenience function that combines:
   - Setting optional custom headers
   - Setting the status
   - Setting Content-Type to "text/html; charset=utf-8"
   - Converting the component to HTML with Component.to_html
   - Marking the response as sent

   ```ocaml
   let home conn =
     let page = Component.div [ Component.text "Hello!" ] in
     Conn.render_component Net.Http.Status.Ok page conn
   ```

   With custom headers (e.g., caching):
   ```ocaml
   let static_page conn =
     Conn.render_component
       ~headers:[ ("Cache-Control", "public, max-age=3600") ]
       Net.Http.Status.Ok
       page
       conn
   ```

   Equivalent to:
   ```ocaml
   conn
   |> Conn.with_status status
   |> Conn.with_header "Content-Type" "text/html; charset=utf-8"
   |> Conn.with_body (Component.to_html component)
   |> Conn.send
   ```
*)
val render_component:
  ?headers:(string * string) list ->
  Net.Http.Status.t ->
  'msg Component.t ->
  t ->
  t

(**
   Render a JSON value as response with proper content-type.

   This is a convenience function that combines:
   - Setting optional custom headers
   - Setting the status
   - Setting Content-Type to "application/json"
   - Converting the JSON to string with Data.Json.to_string
   - Marking the response as sent

   ```ocaml
   let api_users conn =
     let json =
       Data.Json.obj [
         ( "users",
           Data.Json.array [
             Data.Json.string "Alice";
             Data.Json.string "Bob";
           ] );
       ]
     in
     Conn.render_json Net.Http.Status.Ok json conn
   ```

   With custom headers (e.g., CORS):
   ```ocaml
   let api_users conn =
     Conn.render_json
       ~headers:[ ("Access-Control-Allow-Origin", "*") ]
       Net.Http.Status.Ok
       json
       conn
   ```

   Equivalent to:
   ```ocaml
   conn
   |> Conn.with_status status
   |> Conn.with_header "Content-Type" "application/json"
   |> Conn.with_body (Data.Json.to_string json)
   |> Conn.send
   ```
*)
val render_json: ?headers:(string * string) list -> Net.Http.Status.t -> Data.Json.t -> t -> t

(**
   Render plain text response with proper content-type.

   This is a convenience function that combines:
   - Setting optional custom headers
   - Setting the status
   - Setting Content-Type to "text/plain; charset=utf-8"
   - Setting the body content
   - Marking the response as sent

   ```ocaml
   let not_found conn =
     Conn.render_text Net.Http.Status.NotFound "Page not found" conn
   ```

   With custom headers (e.g., caching):
   ```ocaml
   let robots conn =
     Conn.render_text
       ~headers:[ ("Cache-Control", "public, max-age=86400") ]
       Net.Http.Status.Ok
       "User-agent: *\nDisallow: /admin"
       conn
   ```

   Equivalent to:
   ```ocaml
   conn
   |> Conn.with_status status
   |> Conn.with_header "Content-Type" "text/plain; charset=utf-8"
   |> Conn.with_body text
   |> Conn.send
   ```
*)
val render_text: ?headers:(string * string) list -> Net.Http.Status.t -> string -> t -> t

(**
   Redirect to another path with 302 Found status.

   This is a convenience function that combines:
   - Setting optional custom headers
   - Setting 302 Found status
   - Setting Location header
   - Setting empty body
   - Marking the response as sent

   ```ocaml
   let old_path conn =
     Conn.redirect "/new-path" conn
   ```

   With custom headers:
   ```ocaml
   let logout conn =
     Conn.redirect
       ~headers:[ ("Cache-Control", "no-store") ]
       "/login"
       conn
   ```

   For external URLs:
   ```ocaml
   let external conn =
     Conn.redirect "https://example.com" conn
   ```

   Equivalent to:
   ```ocaml
   conn
   |> Conn.with_status Found
   |> Conn.with_header "Location" path
   |> Conn.with_body ""
   |> Conn.send
   ```
*)
val redirect: ?headers:(string * string) list -> string -> t -> t

(** Halt middleware pipeline execution. *)
val halt: t -> t

(** Check if the pipeline is halted. *)
val halted: t -> bool

(** Set path or query parameters used by the router. *)
val set_params: (string * string) list -> t -> t

(** Set parsed body parameters used by body parser middleware. *)
val set_body_params: (string * string) list -> t -> t

(**
   Override the request method.

   Used by method_override middleware to support PUT/PATCH/DELETE from HTML forms.

   ```ocaml
   (* HTML forms can only use GET/POST, but we want DELETE. *)
   let conn = Conn.with_method Net.Http.Method.Delete conn
   ```

   **Note:** This is primarily for internal middleware use.
*)
val with_method: Net.Http.Method.t -> t -> t

(**
   Update the peer connection info.

   Used by remote_ip middleware to set the real client IP.

   **Note:** This is primarily for internal middleware use.
*)
val with_peer: peer -> t -> t

(** Get the underlying socket connection. *)
val socket_conn: t -> Socket_pool.Connection.t option

(**
   Upgrade connection to WebSocket. This halts the middleware pipeline.

   ```ocaml
   let websocket_handler conn =
     let (opts, handler) = LiveView.mount (module MyComponent) conn in
     Conn.upgrade_websocket opts handler conn
   ```
*)
val upgrade_websocket: Channel.Handler.upgrade_opts -> Channel.Handler.t -> t -> t

(** Pending WebSocket upgrade information. *)
type upgrade_info = private {
  opts: Channel.Handler.upgrade_opts;
  handler: Channel.Handler.t;
}

(**
   Get the upgrade info if the connection is upgrading to WebSocket.
   Used internally by the framework.
*)
val get_upgrade: t -> upgrade_info option

(**
   Convert connection to HTTP response.

   If the middleware pipeline finishes without sending or halting, this produces
   a default `404 Not Found` response instead of an empty `200 OK`.
*)
val to_response: t -> Web_server.Response.t

(**
   Typed key for storing middleware-specific data in a connection.
   Each key carries a runtime type witness, so values can be recovered without
   unsafe casts.
*)
type 'a assign_key = 'a Collections.TypedKeyHashMap.key

(**
   Create a fresh typed assignment key.
*)
val assign_key: unit -> 'a assign_key

(**
   Store middleware-specific data in the connection.
   Used by middleware to pass data down the pipeline.
*)
val assign: 'a assign_key -> 'a -> t -> t

(**
   Retrieve typed data stored by `assign`.
*)
val get_assign: 'a assign_key -> t -> 'a option
