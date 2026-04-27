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

  let write_all_with = Socket_pool.Connection.write_all_with

  let send_file_slice = Socket_pool.Connection.send_file_slice
end

module Handler = struct
  let run_pipeline_response = fun app conn ->
    match Testing_app.run_pipeline_response app conn with
    | Web_server.Handler.Response response -> Some response
    | Web_server.Handler.Upgrade _ -> None
end

module LiveViewSession = Liveview.Session
module LiveViewProtocol = Liveview.Protocol
module ChannelHandler = Channel.Handler

module Channel = struct
  type initialization_error = ChannelHandler.initialization_error = ..

  type error = ChannelHandler.error =
    | InitializationFailed of initialization_error
    | UnknownOpcode of int

  type reported_error = ChannelHandler.reported_error

  type ('state, 'error) result = ('state, 'error) ChannelHandler.result =
    | Continue of 'state
    | Push of Http.Ws.Frame.t list * 'state
    | Error of 'error

  let initialize = ChannelHandler.initialize

  let reported_error = ChannelHandler.reported_error

  let reported_error_to_string = ChannelHandler.reported_error_to_string
end

module Http1 = struct
  type header_name_error = Web_server.Http1.header_name_error =
    | EmptyHeaderName
    | InvalidHeaderNameChar of { char: char; index: int }

  type header_value_error = Web_server.Http1.header_value_error =
    | InvalidHeaderValueChar of { char: char; index: int }

  type serialization_error = Web_server.Http1.serialization_error =
    | InvalidHeaderName of { name: string; reason: header_name_error }
    | InvalidHeaderValue of { name: string; value: string; reason: header_value_error }

  type websocket_key_error = Web_server.Http1.websocket_key_error =
    | InvalidBase64
    | InvalidLength of { actual: int; expected: int }

  type websocket_upgrade_error = Web_server.Http1.websocket_upgrade_error =
    | InvalidWebSocketMethod of Std.Net.Http.Method.t
    | InvalidWebSocketVersion of Std.Net.Http.Version.t
    | MissingWebSocketUpgrade
    | InvalidWebSocketUpgrade of { value: string }
    | MissingWebSocketConnectionUpgrade
    | MissingWebSocketVersion
    | UnsupportedWebSocketVersion of { value: string; expected: string }
    | MissingWebSocketKey
    | InvalidWebSocketKey of { value: string; reason: websocket_key_error }

  type content_length_error = Web_server.Http1.content_length_error =
    | InvalidInteger
    | NegativeLength of int

  type request_body_header_error = Web_server.Http1.request_body_header_error =
    | InvalidContentLength of { value: string; reason: content_length_error }
    | ConflictingContentLength of {
        values: string list;
      }
    | ContentLengthExceedsLimit of { length: int; limit: int }
    | TransferEncodingWithContentLength of {
        transfer_encoding: string;
        content_lengths: string list;
      }
    | UnsupportedTransferEncoding of { value: string }

  type request_header_error = Web_server.Http1.request_header_error =
    | MissingHostHeader

  let serialize_response = Web_server.Http1.serialize_response

  let compute_websocket_accept = Web_server.Http1.compute_websocket_accept

  let validate_websocket_upgrade = Web_server.Http1.validate_websocket_upgrade

  let websocket_upgrade_error_to_string = Web_server.Http1.websocket_upgrade_error_to_string

  let validate_request_body_headers = Web_server.Http1.validate_request_body_headers

  let request_body_header_error_to_string = Web_server.Http1.request_body_header_error_to_string

  let split_request_body = Web_server.Http1.split_request_body

  let validate_request_headers = Web_server.Http1.validate_request_headers

  let request_header_error_to_string = Web_server.Http1.request_header_error_to_string

  let should_keep_alive = Web_server.Http1.should_keep_alive

  let should_continue_keep_alive = Web_server.Http1.should_continue_keep_alive
end
