module Config = Config

module Middleware = Middleware

module Component = Component

module LiveView = Liveview

(* User-facing modules *)
module Conn = Middleware.Conn

module Response = Web_server.Response

module Request = Web_server.Request

(* Type aliases for convenience *)
type middleware = Middleware.Pipeline.middleware

type handler = Middleware.Pipeline.t

(* Low-level modules (not exposed in .mli) *)
module SocketPool = Socket_pool

module WebServer = Web_server

module Channel = Channel

module Connection = Socket_pool.Connection

module Handler = Web_server.Handler

(** Suri.config () -> creates configuration with optional parameters *)
let config = fun ?(host = "0.0.0.0") ?(port = 4_000) ?(acceptors = Std.Thread.available_parallelism) ?(max_request_line_length = 8_192) ?(max_header_count = 100) ?(max_header_length = 8_192) ?(buffer_size = 4_096) ?(liveview_secret = "INSECURE-CHANGE-ME-TO-AT-LEAST-32-CHARS") () -> Config.{
  host;
  port;
  acceptors;
  max_request_line_length;
  max_header_count;
  max_header_length;
  buffer_size;
  liveview_secret
}

(**
   Suri.start_link app -> starts the web server

   Handler is just a Middleware.t (a list of Conn.t -> Conn.t functions).
   The middleware pipeline is automatically wrapped to work with the low-level
   WebServer API.
*)
let start_link = fun ?(config = Config.default) (app: Middleware.Pipeline.t) ->
  (* Internal adapter: converts middleware pipeline to low-level handler *)
  let handler socket_conn req =
    let conn = Middleware.Conn.make socket_conn req in
    (* Run the middleware pipeline *)
    let conn = Middleware.Pipeline.run conn app in
    (* Check if this is a WebSocket upgrade *)
    match Middleware.Conn.get_upgrade conn with
    | Some upgrade_info -> (* WebSocket upgrade requested *)
    WebServer.Handler.upgrade upgrade_info.opts upgrade_info.handler
    | None ->
        (* Normal HTTP response *)
        let response = Middleware.Conn.to_response conn in WebServer.Handler.respond response
  in
  (* Convert to internal WebServer config *)
  let web_config = WebServer.Config.make ~max_request_line_length:config.max_request_line_length ~max_header_count:config.max_header_count ~max_header_length:config.max_header_length ~buffer_size:config.buffer_size () in (* Start the web server with our adapted handler *)
  WebServer.start_link ~host:config.host ~port:config.port ~acceptors:config.acceptors ~config:web_config ~handler ()
