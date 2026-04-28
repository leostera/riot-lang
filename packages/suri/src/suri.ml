module Config = Config
module Middleware = Middleware
module Component = Component
module LiveView = Liveview
module Testing = Testing

(* User-facing modules *)

module Conn = Middleware.Conn
module Response = Web_server.Response
module Request = Web_server.Request

(* Type aliases for convenience *)

type middleware = Middleware.Pipeline.middleware

type handler = Middleware.Pipeline.t

type start_error =
  | InvalidConfig of Config.error list
  | InvalidAddress of Std.Net.Addr.error
  | BindFailed of Std.Net.TcpListener.error
  | InvalidAcceptors of int
  | InvalidBufferSize of int

let start_error_of_web_server_error = function
  | Web_server.InvalidAddress error -> InvalidAddress error
  | Web_server.BindFailed error -> BindFailed error
  | Web_server.InvalidAcceptors acceptors -> InvalidAcceptors acceptors
  | Web_server.InvalidBufferSize buffer_size -> InvalidBufferSize buffer_size

let addr_error_to_string = function
  | Std.Net.Addr.System_error error -> Std.IO.error_message error
  | Std.Net.Addr.Invalid_port_number value ->
      Std.String.concat "" [ "invalid port number: "; value; ]
  | Std.Net.Addr.Invalid_format value -> Std.String.concat "" [ "invalid address format: "; value; ]

let listener_error_to_string = function
  | Std.Net.TcpListener.Connection_refused -> "connection refused"
  | Std.Net.TcpListener.Closed -> "listener is closed"
  | Std.Net.TcpListener.System_error error -> Std.IO.error_message error

let start_error_to_string = function
  | InvalidConfig errors -> Config.errors_to_string errors
  | InvalidAddress error -> addr_error_to_string error
  | BindFailed error -> listener_error_to_string error
  | InvalidAcceptors acceptors ->
      Std.String.concat
        ""
        [ "acceptors must be greater than 0, got "; Std.Int.to_string acceptors; ]
  | InvalidBufferSize buffer_size ->
      Std.String.concat
        ""
        [ "buffer_size must be greater than 0, got "; Std.Int.to_string buffer_size; ]

(* Low-level modules (not exposed in .mli) *)

module SocketPool = Socket_pool
module WebServer = Web_server
module Channel = Channel
module Connection = Socket_pool.Connection
module Handler = Web_server.Handler

let internal_server_error_response = fun () ->
  Web_server.Response.internal_server_error
    ~headers:[ ("content-type", "text/plain; charset=utf-8"); ]
    ~body:"Internal Server Error"
    ()

let conn_to_handler_response = fun conn ->
  match Middleware.Conn.get_upgrade conn with
  | Some upgrade_info -> WebServer.Handler.upgrade upgrade_info.opts upgrade_info.handler
  | None -> WebServer.Handler.respond (Middleware.Conn.to_response conn)

let run_app_on_conn = fun app conn ->
  try
    let conn = Middleware.Pipeline.run conn app in
    conn_to_handler_response conn
  with
  | exn ->
      Std.Log.error
        (Std.String.concat
          ""
          [ "Unhandled exception while handling Suri request: "; Std.Exception.to_string exn; ]);
      WebServer.Handler.respond (internal_server_error_response ())

(** Suri.config () -> creates configuration with optional parameters *)
let config = fun
  ?(env = Config.Development)
  ?(host = "0.0.0.0")
  ?(port = 4_000)
  ?(acceptors = Std.Thread.available_parallelism)
  ?(max_request_line_length = 8_192)
  ?(max_header_count = 100)
  ?(max_header_length = 8_192)
  ?(max_body_size = 10_485_760)
  ?(max_keep_alive_requests = 100)
  ?(max_websocket_frame_size = 1_048_576)
  ?(max_websocket_message_size = 16_777_216)
  ?(read_header_timeout_ms = 5_000)
  ?(read_body_timeout_ms = 30_000)
  ?(idle_timeout_ms = 60_000)
  ?(write_timeout_ms = 30_000)
  ?(buffer_size = 4_096)
  ?(liveview_secret = "INSECURE-CHANGE-ME-TO-AT-LEAST-32-CHARS")
  () ->
  let config =
    Config.{
      env;
      host;
      port;
      acceptors;
      max_request_line_length;
      max_header_count;
      max_header_length;
      max_body_size;
      max_keep_alive_requests;
      max_websocket_frame_size;
      max_websocket_message_size;
      read_header_timeout_ms;
      read_body_timeout_ms;
      idle_timeout_ms;
      write_timeout_ms;
      buffer_size;
      liveview_secret;
    }
  in
  Config.validate config

(**
   Suri.start_link app -> starts the web server

   Handler is just a Middleware.t (a list of Conn.t -> Conn.t functions).
   The middleware pipeline is automatically wrapped to work with the low-level
   WebServer API.
*)
let start_link = fun ?(config = Config.default) (app: Middleware.Pipeline.t) ->
  match Config.validate config with
  | Error errors -> Std.Result.err (InvalidConfig errors)
  | Ok config -> (
      (* Internal adapter: converts middleware pipeline to low-level handler *)
      let handler socket_conn req =
        let conn = Middleware.Conn.make socket_conn req in
        run_app_on_conn app conn
      in
      (* Convert to internal WebServer config *)
      let web_config =
        WebServer.Config.make
          ~max_request_line_length:config.max_request_line_length
          ~max_header_count:config.max_header_count
          ~max_header_length:config.max_header_length
          ~max_body_size:config.max_body_size
          ~max_keep_alive_requests:config.max_keep_alive_requests
          ~max_websocket_frame_size:config.max_websocket_frame_size
          ~max_websocket_message_size:config.max_websocket_message_size
          ~read_header_timeout_ms:config.read_header_timeout_ms
          ~read_body_timeout_ms:config.read_body_timeout_ms
          ~idle_timeout_ms:config.idle_timeout_ms
          ~write_timeout_ms:config.write_timeout_ms
          ~buffer_size:config.buffer_size
          ()
      in
      (* Start the web server with our adapted handler *)
      match WebServer.start_link
        ~host:config.host
        ~port:config.port
        ~acceptors:config.acceptors
        ~config:web_config
        ~handler
        () with
      | Ok supervisor -> Std.Result.ok supervisor
      | Error error -> Std.Result.err (start_error_of_web_server_error error)
    )
