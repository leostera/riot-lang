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

type start_error = Web_server.start_error =
  | InvalidAddress of Std.Net.Addr.error
  | BindFailed of Std.Net.TcpListener.error

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

    type error = Socket_pool.Connection.error =
      | Closed
      | FileError of Std.Fs.error
      | InvalidRange of send_file_range_error

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
    type decode_error = Suri__Liveview__Session.decode_error =
      | InvalidTokenFormat
      | InvalidSignature
      | InvalidPayloadBase64
      | InvalidJson of Std.Data.Json.error

    let sign = Suri__Liveview__Session.sign

    let verify = Suri__Liveview__Session.verify

    let encode = Suri__Liveview__Session.encode

    let decode = Suri__Liveview__Session.decode

    let decode_error_to_string = Suri__Liveview__Session.decode_error_to_string
  end

  module LiveViewProtocol = struct
    type client_msg = Suri__Liveview__Protocol.client_msg =
      | Mount
      | Event of { handler_id: string; event_data: string }

    type client_msg_error = Suri__Liveview__Protocol.client_msg_error =
      | InvalidJson of Std.Data.Json.error
      | UnknownMessageFormat of Std.Data.Json.t
      | UnexpectedDecodeException of exn

    let deserialize_client_msg = Suri__Liveview__Protocol.deserialize_client_msg

    let client_msg_error_to_string = Suri__Liveview__Protocol.client_msg_error_to_string
  end

  module Channel = struct
    type initialization_error = Suri__Channel.Handler.initialization_error = ..

    type error = Suri__Channel.Handler.error =
      | InitializationFailed of initialization_error
      | UnknownOpcode of int

    type reported_error = Suri__Channel.Handler.reported_error

    type ('state, 'error) result = ('state, 'error) Suri__Channel.Handler.result =
      | Continue of 'state
      | Push of Http.Ws.Frame.t list * 'state
      | Error of 'error

    let initialize = Suri__Channel.Handler.For_testing.initialize

    let reported_error = Suri__Channel.Handler.For_testing.reported_error

    let reported_error_to_string = Suri__Channel.Handler.For_testing.reported_error_to_string
  end

  module Http1 = struct
    type header_name_error = Web_server.Http1.For_testing.header_name_error =
      | EmptyHeaderName
      | InvalidHeaderNameChar of { char: char; index: int }

    type header_value_error = Web_server.Http1.For_testing.header_value_error =
      | InvalidHeaderValueChar of { char: char; index: int }

    type serialization_error =
      | InvalidHeaderName of { name: string; reason: header_name_error }
      | InvalidHeaderValue of { name: string; value: string; reason: header_value_error }

    type websocket_key_error = Web_server.Http1.For_testing.websocket_key_error =
      | InvalidBase64
      | InvalidLength of { actual: int; expected: int }

    type websocket_upgrade_error = Web_server.Http1.For_testing.websocket_upgrade_error =
      | InvalidWebSocketMethod of Std.Net.Http.Method.t
      | InvalidWebSocketVersion of Std.Net.Http.Version.t
      | MissingWebSocketUpgrade
      | InvalidWebSocketUpgrade of { value: string }
      | MissingWebSocketConnectionUpgrade
      | MissingWebSocketVersion
      | UnsupportedWebSocketVersion of { value: string; expected: string }
      | MissingWebSocketKey
      | InvalidWebSocketKey of { value: string; reason: websocket_key_error }

    type content_length_error = Web_server.Http1.For_testing.content_length_error =
      | InvalidInteger
      | NegativeLength of int

    type request_body_header_error = Web_server.Http1.For_testing.request_body_header_error =
      | InvalidContentLength of { value: string; reason: content_length_error }
      | ConflictingContentLength of {
          values: string list;
        }
      | TransferEncodingWithContentLength of {
          transfer_encoding: string;
          content_lengths: string list;
        }
      | UnsupportedTransferEncoding of { value: string }

    type request_header_error = Web_server.Http1.For_testing.request_header_error =
      | MissingHostHeader

    let serialize_response = fun response ->
      Std.Result.map_err
        (Web_server.Http1.For_testing.serialize_response response)
        ~fn:(
          function
          | Web_server.Http1.For_testing.InvalidHeaderName { name; reason } ->
              InvalidHeaderName { name; reason }
          | Web_server.Http1.For_testing.InvalidHeaderValue { name; value; reason } ->
              InvalidHeaderValue { name; value; reason }
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
