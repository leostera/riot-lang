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

module For_testing = struct
  module Connection = struct
    type send_file_range_error = Socket_pool.Connection.send_file_range_error = {
      off: int;
      len: int;
      size: int;
    }

    let write_all_with = Socket_pool.Connection.For_testing.write_all_with

    let send_file_slice = Socket_pool.Connection.For_testing.send_file_slice
  end

  module Handler = struct
    let run_pipeline_response = fun app conn ->
      match run_app_on_conn app conn with
      | WebServer.Handler.Response response -> Some response
      | WebServer.Handler.Upgrade _ -> None
  end

  module LiveViewSession = struct
    let sign = Suri__Liveview__Session.sign

    let verify = Suri__Liveview__Session.verify

    let encode = Suri__Liveview__Session.encode

    let decode = Suri__Liveview__Session.decode
  end

  module Http1 = struct
    type serialization_error =
      | InvalidHeaderName of string
      | InvalidHeaderValue of { name: string; value: string }

    type websocket_upgrade_error = Web_server.Http1.For_testing.websocket_upgrade_error =
      | InvalidWebSocketMethod of Std.Net.Http.Method.t
      | InvalidWebSocketVersion of Std.Net.Http.Version.t
      | MissingWebSocketUpgrade
      | InvalidWebSocketUpgrade of string
      | MissingWebSocketConnectionUpgrade
      | MissingWebSocketVersion
      | UnsupportedWebSocketVersion of string
      | MissingWebSocketKey
      | InvalidWebSocketKey of string

    type request_body_header_error = Web_server.Http1.For_testing.request_body_header_error =
      | InvalidContentLength of string
      | ConflictingContentLength of string list
      | TransferEncodingWithContentLength
      | UnsupportedTransferEncoding of string

    type request_header_error = Web_server.Http1.For_testing.request_header_error =
      | MissingHostHeader

    let serialize_response = fun response ->
      Std.Result.map_err
        (Web_server.Http1.For_testing.serialize_response response)
        ~fn:(
          function
          | Web_server.Http1.For_testing.InvalidHeaderName name -> InvalidHeaderName name
          | Web_server.Http1.For_testing.InvalidHeaderValue { name; value } ->
              InvalidHeaderValue { name; value }
        )

    let compute_websocket_accept = Web_server.Http1.For_testing.compute_websocket_accept

    let validate_websocket_upgrade = Web_server.Http1.For_testing.validate_websocket_upgrade

    let websocket_upgrade_error_to_string =
      Web_server.Http1.For_testing.websocket_upgrade_error_to_string

    let validate_request_body_headers = Web_server.Http1.For_testing.validate_request_body_headers

    let request_body_header_error_to_string =
      Web_server.Http1.For_testing.request_body_header_error_to_string

    let split_request_body = Web_server.Http1.For_testing.split_request_body

    let validate_request_headers = Web_server.Http1.For_testing.validate_request_headers

    let request_header_error_to_string = Web_server.Http1.For_testing.request_header_error_to_string
  end
end

(** Suri.config () -> creates configuration with optional parameters *)
let config = fun
  ?(env = Config.Development)
  ?(host = "0.0.0.0")
  ?(port = 4_000)
  ?(acceptors = Std.Thread.available_parallelism)
  ?(max_request_line_length = 8_192)
  ?(max_header_count = 100)
  ?(max_header_length = 8_192)
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
      buffer_size;
      liveview_secret;
    }
  in
  match Config.validate config with
  | Ok config -> config
  | Error errors ->
      Std.panic (Std.String.concat "" [ "Invalid Suri config:\n"; Config.errors_to_string errors; ])

(**
   Suri.start_link app -> starts the web server

   Handler is just a Middleware.t (a list of Conn.t -> Conn.t functions).
   The middleware pipeline is automatically wrapped to work with the low-level
   WebServer API.
*)
let start_link = fun ?(config = Config.default) (app: Middleware.Pipeline.t) ->
  let config =
    match Config.validate config with
    | Ok config -> config
    | Error errors ->
        Std.panic
          (Std.String.concat "" [ "Invalid Suri config:\n"; Config.errors_to_string errors; ])
  in
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
      ~buffer_size:config.buffer_size
      ()
  in
  (* Start the web server with our adapted handler *)
  WebServer.start_link
    ~host:config.host
    ~port:config.port
    ~acceptors:config.acceptors
    ~config:web_config
    ~handler
    ()
