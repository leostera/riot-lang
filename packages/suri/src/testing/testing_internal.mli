(**
   White-box helpers for Suri's own subsystem tests.

   Application tests should prefer {!Suri.Testing.Request}, {!Suri.Testing.App},
   {!Suri.Testing.Middleware}, and {!Suri.Testing.Expect}. This module keeps
   framework-internal assertions out of the main public testing API.
*)
module Connection: sig
  type send_file_range_error = { off: int; len: int; size: int }
  type error =
    | Closed
    | ReadError of Std.Net.TcpStream.error
    | WriteError of Std.Net.TcpStream.error
    | FileError of Std.Fs.error
    | InvalidRange of send_file_range_error

  val error_to_string: error -> string

  val write_all_with:
    write:(bytes -> pos:int -> len:int -> (int, Std.Net.TcpStream.error) Std.result) ->
    string ->
    (unit, error) Std.result

  val send_file_slice: ?off:int -> len:int -> string -> (string, error) Std.result
end

module SocketPool: sig
  type error =
    | InvalidAddress of Std.Net.Addr.error
    | BindFailed of Std.Net.TcpListener.error
    | InvalidAcceptors of int
    | InvalidBufferSize of int

  val validate_start_options: acceptors:int -> buffer_size:int -> (unit, error) Std.result
end

module Handler: sig
  val run_pipeline_response:
    Middleware.Pipeline.t ->
    Middleware.Conn.t ->
    Web_server.Response.t option
end

module LiveViewSession: module type of Liveview.Session

module LiveViewProtocol: module type of Liveview.Protocol

module Channel: sig
  type initialization_error = ..
  type error =
    | InitializationFailed of initialization_error
    | UnknownOpcode of int
  type reported_error
  type ('state, 'error) result =
    | Continue of 'state
    | Push of Http.Ws.Frame.t list * 'state
    | Error of 'error

  val initialize: Channel.Handler.t -> (Channel.Handler.t, reported_error) result

  val reported_error: reported_error -> error

  val reported_error_to_string: reported_error -> string
end

module Http1: sig
  type parse_error =
    | UpstreamParseError of Http.Http1.Common.error
  type header_name_error =
    | EmptyHeaderName
    | InvalidHeaderNameChar of { char: char; index: int }
  type header_value_error =
    | InvalidHeaderValueChar of { char: char; index: int }
  type serialization_error =
    | InvalidHeaderName of {
        name: string;
        reason: header_name_error;
      }
    | InvalidHeaderValue of {
        name: string;
        value: string;
        reason: header_value_error;
      }
  type websocket_key_error =
    | InvalidBase64
    | InvalidLength of { actual: int; expected: int }
  type websocket_upgrade_error =
    | InvalidWebSocketMethod of Std.Net.Http.Method.t
    | InvalidWebSocketVersion of Std.Net.Http.Version.t
    | MissingWebSocketUpgrade
    | InvalidWebSocketUpgrade of { value: string }
    | MissingWebSocketConnectionUpgrade
    | MissingWebSocketVersion
    | UnsupportedWebSocketVersion of { value: string; expected: string }
    | MissingWebSocketKey
    | InvalidWebSocketKey of {
        value: string;
        reason: websocket_key_error;
      }
  type websocket_frame_limit_error =
    | WebSocketFrameTooLarge of { size: int; limit: int }
    | WebSocketMessageTooLarge of { size: int; limit: int }
  type content_length_error =
    | InvalidInteger
    | NegativeLength of int
  type request_body_header_error =
    | InvalidContentLength of {
        value: string;
        reason: content_length_error;
      }
    | ConflictingContentLength of {
        values: string list;
      }
    | ContentLengthExceedsLimit of { length: int; limit: int }
    | TransferEncodingWithContentLength of {
        transfer_encoding: string;
        content_lengths: string list;
      }
    | UnsupportedTransferEncoding of { value: string }
  type request_header_error =
    | MissingHostHeader

  val serialize_response: Web_server.Response.t -> (string, serialization_error) Std.result

  (** Wrap an upstream HTTP/1 parser error as a Suri parse error. *)
  val parse_error_from_upstream_error: Http.Http1.Common.error -> parse_error

  val compute_websocket_accept: string -> string

  val validate_websocket_upgrade:
    Web_server.Request.t ->
    (string, websocket_upgrade_error) Std.result

  val websocket_upgrade_error_to_string: websocket_upgrade_error -> string

  val validate_websocket_frame_limits:
    max_frame_size:int ->
    max_message_size:int ->
    Http.Ws.Frame.t ->
    (unit, websocket_frame_limit_error) Std.result

  val websocket_frame_limit_error_to_string: websocket_frame_limit_error -> string

  val validate_request_body_headers:
    ?max_body_size:int ->
    Std.Net.Http.Request.t ->
    (int, request_body_header_error) Std.result

  val request_body_header_error_to_string: request_body_header_error -> string

  val split_request_body: string -> int -> string * string

  val validate_request_headers: Std.Net.Http.Request.t -> (unit, request_header_error) Std.result

  val request_header_error_to_string: request_header_error -> string

  val should_keep_alive: Web_server.Request.t -> bool

  val should_continue_keep_alive:
    max_keep_alive_requests:int ->
    requests_processed:int ->
    Web_server.Request.t ->
    bool
end

module Http2: sig
  type pseudo_header =
    | Method
    | Scheme
    | Path
  type request_header_error =
    | MissingPseudoHeader of pseudo_header
    | EmptyPseudoHeader of pseudo_header
    | InvalidPath of {
        value: string;
        reason: Std.Net.Uri.error;
      }

  val headers_to_request:
    Http.Http2.Hpack.header list ->
    string ->
    (Web_server.Request.t, request_header_error) Std.result

  val request_header_error_to_string: request_header_error -> string
end
