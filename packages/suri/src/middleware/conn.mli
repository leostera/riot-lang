(** # Connection Context

    Rich context object that flows through middleware pipelines.

    A connection represents an HTTP request/response cycle with mutable state
    that middleware can read and modify.

    ## Example

    ```ocaml let handler conn = conn |> Conn.with_status Ok |> Conn.with_body
    "Hello, World!" |> Conn.send ``` *)

open Std

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

val peer : t -> peer
(** Get peer connection info *)

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

(** ## Control Flow *)

val halt : t -> t
(** Halt middleware pipeline execution *)

val halted : t -> bool
(** Check if pipeline is halted *)

(** ## Parameters *)

val set_params : (string * string) list -> t -> t
(** Set path/query parameters (used by router) *)

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
