open Std

(** # Connection Context

    Rich context object that flows through middleware pipelines.

    A connection represents an HTTP request/response cycle with mutable state
    that middleware can read and modify.

    ## Example

    ```ocaml let handler conn = conn |> Conn.with_status Ok |> Conn.with_body
    "Hello, World!" |> Conn.send ``` *)

type peer = { ip : Net.Addr.tcp_addr; port : int }
(** Peer connection information *)

type t
(** Connection context *)

val make : Socket_pool.Connection.t -> Web_server.Request.t -> t
(** Create a new connection from a socket connection and parsed request *)

(** ## Request Access *)

val method_ : t -> Net.Http.Method.t
(** Get the HTTP method *)

val uri : t -> string
(** Get the request URI *)

val path : t -> string
(** Get the request path (without query string) *)

val headers : t -> Net.Http.Header.t
(** Get request headers *)

val body : t -> string
(** Get request body *)

val params : t -> (string * string) list
(** Get path/query parameters *)

val body_params : t -> (string * string) list
(** Get parsed body parameters (set by body_parser middleware) *)

val peer : t -> peer
(** Get peer connection info *)

val resp_headers : t -> (string * string) list
(** Get response headers that have been set so far.
    Useful for reading headers set by upstream middleware. *)

(** ## Response Building *)

val with_status : Net.Http.Status.t -> t -> t
(** Set response status *)

val with_body : string -> t -> t
(** Set response body *)

val with_header : string -> string -> t -> t
(** Add a response header *)

val respond : status:Net.Http.Status.t -> ?body:string -> t -> t
(** Set status and optionally body *)

(** ## Response Sending *)

val send : t -> t
(** Mark connection as ready to send response *)

val sent : t -> bool
(** Check if response has been sent *)

(** ## HTML Rendering *)

val render_component : ?headers:(string * string) list -> Net.Http.Status.t -> 'msg Component.t -> t -> t
(** Render an HTML component as response with proper content-type.
    
    This is a convenience function that combines:
    - Setting optional custom headers
    - Setting the status
    - Setting Content-Type to "text/html; charset=utf-8"
    - Converting the component to HTML with Component.to_html
    - Marking the response as sent
    
    Example:
    {[
      let home conn =
        let page = Component.div [Component.text "Hello!"] in
        Conn.render_component Net.Http.Status.Ok page conn
    ]}
    
    With custom headers (e.g., caching):
    {[
      let static_page conn =
        Conn.render_component 
          ~headers:[("Cache-Control", "public, max-age=3600")]
          Net.Http.Status.Ok 
          page 
          conn
    ]}
    
    Equivalent to:
    {[
      conn
      |> Conn.with_status status
      |> Conn.with_header "Content-Type" "text/html; charset=utf-8"
      |> Conn.with_body (Component.to_html component)
      |> Conn.send
    ]} *)

val render_json : ?headers:(string * string) list -> Net.Http.Status.t -> Data.Json.t -> t -> t
(** Render a JSON value as response with proper content-type.
    
    This is a convenience function that combines:
    - Setting optional custom headers
    - Setting the status
    - Setting Content-Type to "application/json"
    - Converting the JSON to string with Data.Json.to_string
    - Marking the response as sent
    
    Example:
    {[
      let api_users conn =
        let json = Data.Json.obj [
          ("users", Data.Json.array [
            Data.Json.string "Alice";
            Data.Json.string "Bob";
          ])
        ] in
        Conn.render_json Net.Http.Status.Ok json conn
    ]}
    
    With custom headers (e.g., CORS):
    {[
      let api_users conn =
        Conn.render_json 
          ~headers:[("Access-Control-Allow-Origin", "*")]
          Net.Http.Status.Ok 
          json 
          conn
    ]}
    
    Equivalent to:
    {[
      conn
      |> Conn.with_status status
      |> Conn.with_header "Content-Type" "application/json"
      |> Conn.with_body (Data.Json.to_string json)
      |> Conn.send
    ]} *)

(** ## Control Flow *)

val halt : t -> t
(** Halt middleware pipeline execution *)

val halted : t -> bool
(** Check if pipeline is halted *)

(** ## Parameters *)

val set_params : (string * string) list -> t -> t
(** Set path/query parameters (used by router) *)

val set_body_params : (string * string) list -> t -> t
(** Set parsed body parameters (used by body_parser middleware) *)

val with_method : Net.Http.Method.t -> t -> t

val with_peer : peer -> t -> t
(** Update the peer connection info.
    
    Used by remote_ip middleware to set the real client IP.
    
    {b Note}: This is primarily for internal middleware use. *)(** Override the request method.
    
    Used by method_override middleware to support PUT/PATCH/DELETE from HTML forms.
    
    Example:
    {[
      (* HTML forms can only use GET/POST, but we want DELETE *)
      let conn = Conn.with_method Net.Http.Method.Delete conn
    ]}
    
    {b Note}: This is primarily for internal middleware use. *)

val socket_conn : t -> Socket_pool.Connection.t
(** Get the underlying socket connection *)

(** ## WebSocket Upgrade *)

val upgrade_websocket : 
  Channel.Handler.upgrade_opts -> 
  Channel.Handler.t -> 
  t -> 
  t
(** Upgrade connection to WebSocket. This halts the middleware pipeline.
    
    Example:
    {[
      let websocket_handler conn =
        let (opts, handler) = LiveView.mount (module MyComponent) conn in
        Conn.upgrade_websocket opts handler conn
    ]} *)

type upgrade_info = private {
  opts : Channel.Handler.upgrade_opts;
  handler : Channel.Handler.t;
}

val get_upgrade : t -> upgrade_info option
(** Get the upgrade info if the connection is upgrading to WebSocket.
    Used internally by the framework. *)

(** ## Response Extraction *)

val to_response : t -> Web_server.Response.t
(** Convert connection to HTTP response *)

(** ## Private Data Storage *)

type assign_value = ..
(** Extensible type for storing arbitrary data in connection.
    Middleware can extend this type to store their own data. *)

val assign : string -> assign_value -> t -> unit
(** Store arbitrary data in the connection.
    Used by middleware to pass data down the pipeline. *)

val get_assign : string -> t -> assign_value option
(** Retrieve data stored by [assign]. *)
