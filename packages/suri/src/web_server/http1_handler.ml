module Web_config = Config

open Std

type parse_state =
  | WaitingForHeaders
  | WaitingForBody of {
      http_req: Net.Http.Request.t;
      expected_length: int;
      accumulated_body: string;
    }

type state = {
  config: Web_config.t;
  handler: Http_handler.t;
  is_keep_alive: bool;
  requests_processed: int;
  sniffed_data: string;
  parse_state: parse_state;
}

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

type io_error =
  | ResponseSerializationFailed of serialization_error
  | ConnectionFailed of Socket_pool.Connection.error

type parse_error =
  | UpstreamParseError of Http.Http1.Common.error

type error =
  | ParseError of parse_error
  | ExcessBodyRead
  | IoError of io_error

type websocket_key_error =
  | InvalidBase64
  | InvalidLength of { actual: int; expected: int }

type websocket_upgrade_error =
  | InvalidWebSocketMethod of Net.Http.Method.t
  | InvalidWebSocketVersion of Net.Http.Version.t
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

type websocket_bridge_error =
  | WebSocketChannelError of Channel.Handler.reported_error
  | WebSocketParseFailed of Http.Ws.Parser.error
  | WebSocketMessageFailed of Http.Ws.Message.error
  | WebSocketSerializeFailed of Http.Ws.Serializer.error
  | WebSocketFrameLimitFailed of websocket_frame_limit_error

type websocket_state = {
  ws_handler: Channel.Handler.t;
  pending_data: string;
  message_state: Http.Ws.Message.t;
}

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

let header_name_error_to_string = fun __tmp1 ->
  match __tmp1 with
  | EmptyHeaderName -> "header name must not be empty"
  | InvalidHeaderNameChar { char; index } ->
      "invalid header name character code "
      ^ Int.to_string (Char.code char)
      ^ " at index "
      ^ Int.to_string index

let header_value_error_to_string = fun (InvalidHeaderValueChar { char; index }) ->
  "invalid header value character code "
  ^ Int.to_string (Char.code char)
  ^ " at index "
  ^ Int.to_string index

let serialization_error_to_string = fun __tmp1 ->
  match __tmp1 with
  | InvalidHeaderName { name; reason } ->
      "Invalid response header name: " ^ name ^ " (" ^ header_name_error_to_string reason ^ ")"
  | InvalidHeaderValue { name; value = _; reason } ->
      "Invalid response header value for: "
      ^ name
      ^ " ("
      ^ header_value_error_to_string reason
      ^ ")"

let parse_error_of_upstream_error = fun error -> UpstreamParseError error

let parse_error_to_string = fun (UpstreamParseError error) ->
  Http.Http1.Common.error_to_string
    error

let websocket_upgrade_error_to_string = fun __tmp1 ->
  match __tmp1 with
  | InvalidWebSocketMethod method_ ->
      "WebSocket upgrade requests must use GET, got " ^ Net.Http.Method.to_string method_
  | InvalidWebSocketVersion version ->
      "WebSocket upgrade requests require HTTP/1.1 or newer, got "
      ^ Net.Http.Version.to_string version
  | MissingWebSocketUpgrade -> "Missing Upgrade: websocket header"
  | InvalidWebSocketUpgrade { value } -> "Invalid Upgrade header for WebSocket request: " ^ value
  | MissingWebSocketConnectionUpgrade -> "Missing Connection: Upgrade token for WebSocket request"
  | MissingWebSocketVersion -> "Missing Sec-WebSocket-Version header"
  | UnsupportedWebSocketVersion { value; expected } ->
      "Unsupported WebSocket version: " ^ value ^ "; expected " ^ expected
  | MissingWebSocketKey -> "Missing Sec-WebSocket-Key header"
  | InvalidWebSocketKey { reason = InvalidBase64; _ } -> "Invalid Sec-WebSocket-Key header; expected base64 for exactly 16 bytes"
  | InvalidWebSocketKey { reason = InvalidLength { actual; expected }; _ } ->
      "Invalid Sec-WebSocket-Key header; expected "
      ^ Int.to_string expected
      ^ " bytes, got "
      ^ Int.to_string actual

let websocket_frame_limit_error_to_string = fun __tmp1 ->
  match __tmp1 with
  | WebSocketFrameTooLarge { size; limit } ->
      "WebSocket frame payload is too large: "
      ^ Int.to_string size
      ^ " bytes, maximum is "
      ^ Int.to_string limit
      ^ " bytes"
  | WebSocketMessageTooLarge { size; limit } ->
      "WebSocket message payload is too large: "
      ^ Int.to_string size
      ^ " bytes, maximum is "
      ^ Int.to_string limit
      ^ " bytes"

let websocket_bridge_error_to_string = fun __tmp1 ->
  match __tmp1 with
  | WebSocketChannelError error -> Channel.Handler.reported_error_to_string error
  | WebSocketParseFailed error ->
      "WebSocket frame parse error: " ^ Http.Ws.Parser.error_to_string error
  | WebSocketMessageFailed error ->
      "WebSocket message error: " ^ Http.Ws.Message.error_to_string error
  | WebSocketSerializeFailed error ->
      "WebSocket frame serialize error: " ^ Http.Ws.Serializer.error_to_string error
  | WebSocketFrameLimitFailed error -> websocket_frame_limit_error_to_string error

let request_body_header_error_to_string = fun __tmp1 ->
  match __tmp1 with
  | InvalidContentLength { value; reason = InvalidInteger } ->
      "Invalid Content-Length header: " ^ value
  | InvalidContentLength { value; reason = NegativeLength length } ->
      "Invalid Content-Length header: "
      ^ value
      ^ "; length must be non-negative, got "
      ^ Int.to_string length
  | ConflictingContentLength { values } ->
      "Conflicting Content-Length headers: " ^ String.concat ", " values
  | ContentLengthExceedsLimit { length; limit } ->
      "Request body is too large: content-length is "
      ^ Int.to_string length
      ^ " bytes, maximum is "
      ^ Int.to_string limit
      ^ " bytes"
  | TransferEncodingWithContentLength { transfer_encoding; content_lengths } ->
      "Request must not include both Transfer-Encoding ("
      ^ transfer_encoding
      ^ ") and Content-Length ("
      ^ String.concat ", " content_lengths
      ^ ")"
  | UnsupportedTransferEncoding { value } -> "Unsupported Transfer-Encoding header: " ^ value

let request_body_header_error_response = fun error ->
  match error with
  | ContentLengthExceedsLimit _ ->
      Response.request_entity_too_large ~body:(request_body_header_error_to_string error) ()
  | InvalidContentLength _
  | ConflictingContentLength _
  | TransferEncodingWithContentLength _
  | UnsupportedTransferEncoding _ ->
      Response.bad_request ~body:(request_body_header_error_to_string error) ()

let request_header_error_to_string = fun MissingHostHeader ->
  "HTTP/1.1 requests must include a Host header"

let connection_error_to_string = fun __tmp1 ->
  match __tmp1 with
  | Socket_pool.Connection.Closed -> "Connection closed"
  | Socket_pool.Connection.FileError _ -> "Connection file operation failed"
  | Socket_pool.Connection.InvalidRange { off; len; size } ->
      "Invalid connection file range: off="
      ^ Int.to_string off
      ^ ", len="
      ^ Int.to_string len
      ^ ", size="
      ^ Int.to_string size

let io_error_to_string = fun __tmp1 ->
  match __tmp1 with
  | ResponseSerializationFailed error -> serialization_error_to_string error
  | ConnectionFailed error -> connection_error_to_string error

let to_string_error = fun __tmp1 ->
  match __tmp1 with
  | ParseError error -> "Parse error: " ^ parse_error_to_string error
  | ExcessBodyRead -> "Excess body read"
  | IoError error -> "I/O error: " ^ io_error_to_string error

let make_handler = fun ~config ~handler ?(sniffed_data = "") () ->
  {
    config;
    handler;
    sniffed_data;
    is_keep_alive = false;
    requests_processed = 0;
    parse_state = WaitingForHeaders;
  }

let handle_close = fun _conn _state -> ()

let handle_connection = fun _conn state -> Socket_pool.Handler.Continue state

let header_value_has_token = fun value ~token ->
  let expected = String.lowercase_ascii token in
  value
  |> String.split_on_char ','
  |> List.exists
    (fun candidate -> String.equal (String.lowercase_ascii (String.trim candidate)) expected)

let headers_have_token = fun headers name ~token ->
  List.exists
    (header_value_has_token ~token)
    (Net.Http.Header.get_all headers name)

let should_keep_alive = fun (req: Request.t) ->
  let headers = Request.headers req in
  match Request.version req with
  | _ when headers_have_token headers "connection" ~token:"close" -> false
  | _ when headers_have_token headers "connection" ~token:"keep-alive" -> true
  | Net.Http.Version.Http11 -> true
  | _ -> false

let should_continue_keep_alive = fun ~max_keep_alive_requests ~requests_processed req ->
  should_keep_alive req && requests_processed < max_keep_alive_requests

let is_header_name_char = fun __tmp1 ->
  match __tmp1 with
  | 'a' .. 'z'
  | 'A' .. 'Z'
  | '0' .. '9'
  | '!'
  | '#'
  | '$'
  | '%'
  | '&'
  | '\''
  | '*'
  | '+'
  | '-'
  | '.'
  | '^'
  | '_'
  | '`'
  | '|'
  | '~' -> true
  | _ -> false

let validate_header_name = fun name ->
  let rec go index =
    if index >= String.length name then
      Ok ()
    else
      let char = String.get_unchecked name ~at:index in
      if is_header_name_char char then
        go (index + 1)
      else
        Error (InvalidHeaderNameChar { char; index })
  in
  if String.length name = 0 then
    Error EmptyHeaderName
  else
    go 0

let validate_header_value = fun value ->
  let rec go index =
    if index >= String.length value then
      Ok ()
    else
      match String.get_unchecked value ~at:index with
      | ('\r' | '\n') as char -> Error (InvalidHeaderValueChar { char; index })
      | char when Char.code char < 32 && not (char = '\t') ->
          Error (InvalidHeaderValueChar { char; index })
      | _ -> go (index + 1)
  in
  go 0

let validate_response_headers = fun headers ->
  let rec go = fun __tmp1 ->
    match __tmp1 with
    | [] -> Ok ()
    | (name, value) :: rest ->
        match validate_header_name name with
        | Error reason -> Error (InvalidHeaderName { name; reason })
        | Ok () -> (
            match validate_header_value value with
            | Error reason -> Error (InvalidHeaderValue { name; value; reason })
            | Ok () -> go rest
          )
  in
  go (Net.Http.Header.to_list headers)

let response_allows_body = fun status ->
  let code = Net.Http.Status.to_int status in
  not ((code >= 100 && code < 200) || code = 204 || code = 304)

let serialize_response = fun (res: Response.t) ->
  let body =
    if response_allows_body res.status then
      res.body
    else
      ""
  in
  let headers =
    if response_allows_body res.status then
      if Net.Http.Header.has res.headers "content-length" then
        res.headers
      else
        Net.Http.Header.set
          res.headers
          "content-length"
          (
            String.length body
            |> Int.to_string
          )
    else
      Net.Http.Header.remove res.headers "content-length"
  in
  match validate_response_headers headers with
  | Error err -> Error err
  | Ok () ->
      let status_line =
        (Net.Http.Version.to_string res.version)
        ^ " "
        ^ (Int.to_string (Net.Http.Status.to_int res.status))
        ^ " "
        ^ (Net.Http.Status.to_string res.status)
        ^ "\r\n"
      in
      let header_lines =
        Net.Http.Header.to_list headers
        |> List.map ~fn:(fun (k, v) -> k ^ ": " ^ v ^ "\r\n")
        |> String.concat ""
      in
      Ok (status_line ^ header_lines ^ "\r\n" ^ body)

let send_response = fun conn res ->
  match serialize_response res with
  | Error err -> Error (IoError (ResponseSerializationFailed err))
  | Ok response_bytes -> (
      match Socket_pool.Connection.send conn response_bytes with
      | Ok () -> Ok ()
      | Error error -> Error (IoError (ConnectionFailed error))
    )

let compute_websocket_accept = fun key ->
  let magic = "258EAFA5-E914-47DA-95CA-C5AB0DC85B11" in
  let concat = key ^ magic in
  let hash = Crypto.Sha1.hash_string concat in
  let hash_bytes = Crypto.Hash.to_bytes hash in
  Encoding.Base64.encode_bytes hash_bytes

let decode_websocket_key = fun key ->
  match Encoding.Base64.decode key with
  | Ok decoded -> Ok decoded
  | Error Encoding.Base64.InvalidBase64 -> Error InvalidBase64

let validate_websocket_key = fun key ->
  match decode_websocket_key key with
  | Ok decoded ->
      let actual = String.length decoded in
      if actual = 16 then
        Ok ()
      else
        Error (InvalidLength { actual; expected = 16 })
  | Error error -> Error error

let version_supports_websocket_upgrade = fun __tmp1 ->
  match __tmp1 with
  | Net.Http.Version.Http09
  | Net.Http.Version.Http10 -> false
  | Net.Http.Version.Http11
  | Net.Http.Version.Http2
  | Net.Http.Version.Http3 -> true

let validate_websocket_upgrade = fun req ->
  let headers = Request.headers req in
  let method_ = Request.method_ req in
  let version = Request.version req in
  if not (Net.Http.Method.equal method_ Net.Http.Method.Get) then
    Error (InvalidWebSocketMethod method_)
  else if not (version_supports_websocket_upgrade version) then
    Error (InvalidWebSocketVersion version)
  else
    match Net.Http.Header.get headers "upgrade" with
    | None -> Error MissingWebSocketUpgrade
    | Some upgrade when not
      (String.equal (String.lowercase_ascii (String.trim upgrade)) "websocket") ->
        Error (InvalidWebSocketUpgrade { value = upgrade })
    | Some _ -> (
        match headers_have_token headers "connection" ~token:"upgrade" with
        | true -> (
            match Net.Http.Header.get headers "sec-websocket-version" with
            | None -> Error MissingWebSocketVersion
            | Some ws_version when not (String.equal (String.trim ws_version) "13") ->
                Error (UnsupportedWebSocketVersion { value = ws_version; expected = "13" })
            | Some _ -> (
                match Net.Http.Header.get headers "sec-websocket-key" with
                | None -> Error MissingWebSocketKey
                | Some key -> (
                    match validate_websocket_key key with
                    | Ok () -> Ok key
                    | Error reason -> Error (InvalidWebSocketKey { value = key; reason })
                  )
              )
          )
        | false -> Error MissingWebSocketConnectionUpgrade
      )

let all_equal = fun __tmp1 ->
  match __tmp1 with
  | []
  | [ _ ] -> true
  | first :: rest -> List.all rest ~fn:(String.equal first)

let validate_request_body_headers = fun ?max_body_size http_req ->
  let headers = Net.Http.Request.headers http_req in
  let content_lengths = Net.Http.Header.get_all headers "content-length" in
  let check_limit = fun length ->
    match max_body_size with
    | Some limit when length > limit -> Error (ContentLengthExceedsLimit { length; limit })
    | Some _
    | None -> Ok length
  in
  match Net.Http.Header.get headers "transfer-encoding" with
  | Some transfer_encoding ->
      if not (List.is_empty content_lengths) then
        Error (TransferEncodingWithContentLength { transfer_encoding; content_lengths })
      else
        Error (UnsupportedTransferEncoding { value = transfer_encoding })
  | None -> (
      match content_lengths with
      | [] -> Ok 0
      | value :: _ when not (all_equal content_lengths) ->
          Error (ConflictingContentLength { values = content_lengths })
      | value :: _ -> (
          match Int.of_string_opt (String.trim value) with
          | Some len when len >= 0 -> check_limit len
          | Some len -> Error (InvalidContentLength { value; reason = NegativeLength len })
          | None -> Error (InvalidContentLength { value; reason = InvalidInteger })
        )
    )

let split_request_body = fun data expected_length ->
  let body_length = String.length data in
  if body_length <= expected_length then
    (data, "")
  else
    let body = String.sub data ~offset:0 ~len:expected_length in
    let remaining = String.sub data ~offset:expected_length ~len:(body_length - expected_length) in
    (body, remaining)

let validate_request_headers = fun http_req ->
  match Net.Http.Request.version http_req with
  | Net.Http.Version.Http11 ->
      if Net.Http.Request.has_header http_req "host" then
        Ok ()
      else
        Error MissingHostHeader
  | Net.Http.Version.Http09
  | Net.Http.Version.Http10
  | Net.Http.Version.Http2
  | Net.Http.Version.Http3 -> Ok ()

let validate_websocket_frame_limits = fun ~max_frame_size ~max_message_size frame ->
  let size = String.length frame.Http.Ws.Frame.payload in
  if size > max_frame_size then
    Error (WebSocketFrameTooLarge { size; limit = max_frame_size })
  else
    match frame.opcode with
    | Http.Ws.Frame.Text
    | Http.Ws.Frame.Binary
    | Http.Ws.Frame.Continuation when size > max_message_size ->
        Error (WebSocketMessageTooLarge { size; limit = max_message_size })
    | Http.Ws.Frame.Text
    | Http.Ws.Frame.Binary
    | Http.Ws.Frame.Continuation
    | Http.Ws.Frame.Close
    | Http.Ws.Frame.Ping
    | Http.Ws.Frame.Pong -> Ok ()

(* Bridge Channel.Handler.t to Socket_pool.Handler.t for WebSocket connections *)

let serialize_websocket_frames = fun frames ->
  let rec loop frames acc =
    match frames with
    | [] -> Ok (String.concat "" (List.rev acc))
    | frame :: rest -> (
        match Http.Ws.Serializer.serialize ~role:Http.Ws.Serializer.Server frame with
        | Ok bytes -> loop rest (bytes :: acc)
        | Error error -> Error error
      )
  in
  loop frames []

let send_websocket_frames = fun conn frames state ->
  match serialize_websocket_frames frames with
  | Error error -> Socket_pool.Handler.Error (state, WebSocketSerializeFailed error)
  | Ok frame_data -> (
      match Socket_pool.Connection.send conn frame_data with
      | Ok () -> Socket_pool.Handler.Continue state
      | Error Socket_pool.Connection.Closed -> Socket_pool.Handler.Close state
      | Error (Socket_pool.Connection.FileError _ | Socket_pool.Connection.InvalidRange _) ->
          Socket_pool.Handler.Close state
    )

let websocket_event_to_frame = fun __tmp1 ->
  match __tmp1 with
  | Http.Ws.Message.ControlFrame frame -> frame
  | Http.Ws.Message.DataMessage { opcode = Text; payload } -> Http.Ws.Frame.text payload
  | Http.Ws.Message.DataMessage { opcode = Binary; payload } -> Http.Ws.Frame.binary payload

let handle_websocket_channel_frame = fun conn stream state frame ->
  match Channel.Handler.handle_frame state.ws_handler frame stream with
  | Channel.Handler.Continue ws_handler -> Socket_pool.Handler.Continue { state with ws_handler }
  | Channel.Handler.Push (out_frames, ws_handler) ->
      send_websocket_frames conn out_frames { state with ws_handler }
  | Channel.Handler.Error err -> Socket_pool.Handler.Error (state, WebSocketChannelError err)

let rec handle_websocket_input = fun config conn stream state input ->
  match Http.Ws.Parser.parse
    ~max_payload_length:config.Web_config.max_websocket_frame_size
    ~role:Http.Ws.Parser.Server
    input with
  | Need_more -> Socket_pool.Handler.Continue { state with pending_data = input }
  | Error error -> Socket_pool.Handler.Error (state, WebSocketParseFailed error)
  | Done { value = frame; remaining } -> (
      match validate_websocket_frame_limits
        ~max_frame_size:config.max_websocket_frame_size
        ~max_message_size:config.max_websocket_message_size
        frame with
      | Error error -> Socket_pool.Handler.Error (state, WebSocketFrameLimitFailed error)
      | Ok () -> (
          match Http.Ws.Message.handle_frame state.message_state frame with
          | Error error -> Socket_pool.Handler.Error (state, WebSocketMessageFailed error)
          | Ok (message_state, None) ->
              let state = { state with message_state; pending_data = "" } in
              if String.length remaining = 0 then
                Socket_pool.Handler.Continue state
              else
                handle_websocket_input config conn stream state remaining
          | Ok (message_state, Some event) -> (
              let frame = websocket_event_to_frame event in
              let state = { state with message_state; pending_data = "" } in
              match handle_websocket_channel_frame conn stream state frame with
              | Socket_pool.Handler.Continue state ->
                  if String.length remaining = 0 then
                    Socket_pool.Handler.Continue state
                  else
                    handle_websocket_input config conn stream state remaining
              | Socket_pool.Handler.Close state -> Socket_pool.Handler.Close state
              | Socket_pool.Handler.Error (state, error) -> Socket_pool.Handler.Error (state, error)
              | Socket_pool.Handler.Ok -> Socket_pool.Handler.Ok
              | Socket_pool.Handler.Switch handler -> Socket_pool.Handler.Switch handler
            )
        )
    )

let websocket_to_socket_pool_handler:
  config:Web_config.t ->
  Channel.Handler.t ->
  (Socket_pool.Handler.t, Http.Ws.Message.error) result = fun ~config ws_handler ->
  match Http.Ws.Message.create ~max_message_size:config.max_websocket_message_size () with
  | Error error -> Error error
  | Ok message_state ->
      let initial_state = { ws_handler; pending_data = ""; message_state } in
      let handler = {
        Socket_pool.Handler.to_string_error = websocket_bridge_error_to_string;
        handle_close = (fun _conn _state -> ());
        handle_connection =
          (fun conn state ->
            (* Initialize the WebSocket handler *)
            Log.info "WebSocket bridge: handle_connection called, initializing Channel handler";
            match Channel.Handler.init state.ws_handler (Socket_pool.Connection.stream conn) with
            | Channel.Handler.Continue new_handler ->
                Log.info "WebSocket bridge: Channel handler initialized successfully";
                Socket_pool.Handler.Continue { state with ws_handler = new_handler }
            | Channel.Handler.Push (out_frames, new_handler) ->
                let result =
                  send_websocket_frames conn out_frames { state with ws_handler = new_handler }
                in
                (
                  match result with
                  | Socket_pool.Handler.Continue _ ->
                      Log.info "WebSocket bridge: Channel handler initialized successfully"
                  | _ -> ()
                );
                result
            | Channel.Handler.Error err ->
                Log.error "WebSocket bridge: Channel handler initialization failed";
                Socket_pool.Handler.Error (state, WebSocketChannelError err));
        handle_data =
          (fun data conn state ->
            (* Parse WebSocket frames from incoming data *)
            let stream = Socket_pool.Connection.stream conn in
            handle_websocket_input config conn stream state (state.pending_data ^ data));
        handle_error = (fun err _conn state -> Socket_pool.Handler.Error (state, err));
        handle_shutdown = (fun _conn state -> Socket_pool.Handler.Close state);
        handle_message =
          (fun msg conn state ->
            let stream = Socket_pool.Connection.stream conn in
            match Channel.Handler.handle_message state.ws_handler msg stream with
            | Channel.Handler.Continue new_handler ->
                Socket_pool.Handler.Continue { state with ws_handler = new_handler }
            | Channel.Handler.Push (frames, new_handler) ->
                send_websocket_frames conn frames { state with ws_handler = new_handler }
            | Channel.Handler.Error err ->
                Socket_pool.Handler.Error (state, WebSocketChannelError err));
      }
      in
      Ok (Socket_pool.Handler.H { handler; state = initial_state })

let handle_websocket_upgrade = fun state socket_conn req ws_handler ->
  match validate_websocket_upgrade req with
  | Error error ->
      let res = Response.bad_request ~body:(websocket_upgrade_error_to_string error) () in
      let _ = send_response socket_conn res in
      Socket_pool.Handler.Close state
  | Ok key ->
      (* Compute accept key *)
      let accept_key = compute_websocket_accept key in
      (* Send 101 Switching Protocols response *)
      let response_headers =
        Net.Http.Header.empty
        |> (fun h -> Net.Http.Header.set h "Upgrade" "websocket")
        |> (fun h -> Net.Http.Header.set h "Connection" "Upgrade")
        |> (fun h -> Net.Http.Header.set h "Sec-WebSocket-Accept" accept_key)
      in
      let status_line = "HTTP/1.1 101 Switching Protocols\r\n" in
      let header_lines =
        Net.Http.Header.to_list response_headers
        |> List.map ~fn:(fun (k, v) -> k ^ ": " ^ v ^ "\r\n")
        |> String.concat ""
      in
      let response_bytes = status_line ^ header_lines ^ "\r\n" in
      Log.info
        ("Sending WebSocket upgrade response ("
        ^ string_of_int (String.length response_bytes)
        ^ " bytes)");
      match websocket_to_socket_pool_handler ~config:state.config ws_handler with
      | Error error ->
          let res =
            Response.internal_server_error ~body:(Http.Ws.Message.error_to_string error) ()
          in
          let _ = send_response socket_conn res in
          Socket_pool.Handler.Close state
      | Ok socket_pool_handler -> (
          match Socket_pool.Connection.send socket_conn response_bytes with
          | Ok () ->
              Log.info "WebSocket upgrade response sent successfully, switching protocols";
              Socket_pool.Handler.Switch socket_pool_handler
          | Error Socket_pool.Connection.Closed ->
              Log.error "Failed to send WebSocket upgrade response - connection closed";
              Socket_pool.Handler.Close state
          | Error (Socket_pool.Connection.FileError _ | Socket_pool.Connection.InvalidRange _) ->
              Log.error "Failed to send WebSocket upgrade response";
              Socket_pool.Handler.Close state
        )

let handle_request = fun state socket_conn (req: Request.t) ->
  match state.handler socket_conn req with
  | Http_handler.Response res -> (
      match send_response socket_conn res with
      | Ok () ->
          let is_keep_alive = should_keep_alive req in
          let requests_processed = state.requests_processed + 1 in
          let new_state = {
            state with
            is_keep_alive;
            requests_processed;
            parse_state = WaitingForHeaders;
          }
          in
          if
            should_continue_keep_alive
              ~max_keep_alive_requests:state.config.max_keep_alive_requests
              ~requests_processed
              req
          then
            Socket_pool.Handler.Continue new_state
          else
            Socket_pool.Handler.Close new_state
      | Error err -> Socket_pool.Handler.Error (state, err)
    )
  | Http_handler.Upgrade (Http_handler.WebSocket (_opts, ws_handler)) ->
      (* WebSocket upgrade *)
      Log.info
        "Http1_handler.handle_request: Matched WebSocket upgrade, calling handle_websocket_upgrade";
      handle_websocket_upgrade state socket_conn req ws_handler

let handle_data_waiting_headers = fun full_data conn state ->
  match Http.Http1.Request.parse
    ~max_request_line:state.config.max_request_line_length
    ~max_headers:state.config.max_header_count
    ~max_header_length:state.config.max_header_length
    full_data with
  | Done { value = http_req; remaining } -> (
      match validate_request_headers http_req with
      | Error error ->
          let res = Response.bad_request ~body:(request_header_error_to_string error) () in
          let _ = send_response conn res in
          Socket_pool.Handler.Close state
      | Ok () -> (
          match validate_request_body_headers ~max_body_size:state.config.max_body_size http_req with
          | Error error ->
              let res = request_body_header_error_response error in
              let _ = send_response conn res in
              Socket_pool.Handler.Close state
          | Ok expected_length ->
              let body_received = String.length remaining in
              if body_received >= expected_length then
                let (body, remaining_data) = split_request_body remaining expected_length in
                let req = Request.from_http ~body http_req in
                handle_request
                  {
                    state with
                    parse_state = WaitingForHeaders;
                    sniffed_data = remaining_data;
                  }
                  conn
                  req
              else
                (* Need to read more body data - transition to WaitingForBody state *)
                Socket_pool.Handler.Continue {
                  state with
                  sniffed_data = "";
                  parse_state = WaitingForBody {
                    http_req;
                    expected_length;
                    accumulated_body = remaining;
                  };
                }
        )
    )
  | Need_more -> Socket_pool.Handler.Continue { state with sniffed_data = full_data }
  | Error upstream_error ->
      let error = parse_error_of_upstream_error upstream_error in
      let res = Response.bad_request ~body:(parse_error_to_string error) () in
      let _ = send_response conn res in
      Socket_pool.Handler.Close state

let handle_data_waiting_body = fun data conn state http_req expected_length accumulated_body ->
  let new_body = accumulated_body ^ data in
  let body_length = String.length new_body in
  if body_length >= expected_length then
    let (complete_body, remaining_data) = split_request_body new_body expected_length in
    (* Process the request with complete body *)
    let req = Request.from_http ~body:complete_body http_req in
    let result =
      handle_request
        {
          state with
          parse_state = WaitingForHeaders;
          sniffed_data = remaining_data;
        }
        conn
        req
    in
    (* If there's remaining data and we're keeping the connection alive, it might be the start of the next request *)
    result
  else
    (* Still need more data *)
    Socket_pool.Handler.Continue {
      state with
      parse_state = WaitingForBody { http_req; expected_length; accumulated_body = new_body };
    }

let handle_data = fun data conn state ->
  match state.parse_state with
  | WaitingForHeaders ->
      let full_data = state.sniffed_data ^ data in
      handle_data_waiting_headers full_data conn state
  | WaitingForBody { http_req; expected_length; accumulated_body } ->
      handle_data_waiting_body data conn state http_req expected_length accumulated_body

let handle_error = fun err _conn state ->
  Log.error ("HTTP/1.1 error: " ^ (to_string_error err));
  Socket_pool.Handler.Close state

let handle_shutdown = fun _conn state -> Socket_pool.Handler.Close state

let handle_message = fun _msg _conn state -> Socket_pool.Handler.Continue state
