open Std

module Component = Suri.Component
module Accepts = Suri.Middleware.Accepts
module Basic_auth = Suri.Middleware.Basic_auth
module Body_parser = Suri.Middleware.Body_parser
module Config = Suri.Config
module Conn = Suri.Middleware.Conn
module Cors = Suri.Middleware.Cors
module Csrf = Suri.Middleware.Csrf
module Logger = Suri.Middleware.Logger
module Remote_ip = Suri.Middleware.Remote_ip
module Request_id = Suri.Middleware.Request_id
module Router = Suri.Middleware.Router
module Session = Suri.Middleware.Session
module Static = Suri.Middleware.Static
module Response = Suri.Response
module Connection = Suri.Testing.Internal.Connection
module Handler = Suri.Testing.Internal.Handler
module LiveViewSession = Suri.Testing.Internal.LiveViewSession
module LiveViewProtocol = Suri.Testing.Internal.LiveViewProtocol
module Channel = Suri.Testing.Internal.Channel
module Http1 = Suri.Testing.Internal.Http1

let valid_websocket_key = "dGhlIHNhbXBsZSBub25jZQ=="

let websocket_request = fun
  ?(method_ = Net.Http.Method.Get)
  ?(version = Net.Http.Version.Http11)
  ?(headers = [("upgrade", "websocket"); ("connection", "keep-alive, Upgrade"); ("sec-websocket-version", "13"); ("sec-websocket-key", valid_websocket_key);])
  () ->
  let uri =
    Net.Uri.of_string "/"
    |> Result.unwrap
  in
  let http_req =
    Net.Http.Request.create method_ uri
    |> fun req ->
      Net.Http.Request.with_version req version
      |> fun req ->
        List.fold_left
          headers
          ~init:req
          ~fn:(fun req (name, value) ->
            Net.Http.Request.with_header req name value)
  in
  Suri.Request.from_http ~body:"" http_req

let http_request = fun
  ?(method_ = Net.Http.Method.Get) ?(version = Net.Http.Version.Http11) ?(headers = []) () ->
  let uri =
    Net.Uri.of_string "/"
    |> Result.unwrap
  in
  Net.Http.Request.create method_ uri
  |> fun req ->
    Net.Http.Request.with_version req version
    |> fun req ->
      List.fold_left
        headers
        ~init:req
        ~fn:(fun req (name, value) ->
          Net.Http.Request.add_header req name value)

let config_for_test = fun
  ?(env = Config.default.env)
  ?(host = Config.default.host)
  ?(port = Config.default.port)
  ?(acceptors = Config.default.acceptors)
  ?(max_request_line_length = Config.default.max_request_line_length)
  ?(max_header_count = Config.default.max_header_count)
  ?(max_header_length = Config.default.max_header_length)
  ?(max_body_size = Config.default.max_body_size)
  ?(max_keep_alive_requests = Config.default.max_keep_alive_requests)
  ?(max_websocket_frame_size = Config.default.max_websocket_frame_size)
  ?(max_websocket_message_size = Config.default.max_websocket_message_size)
  ?(read_header_timeout_ms = Config.default.read_header_timeout_ms)
  ?(read_body_timeout_ms = Config.default.read_body_timeout_ms)
  ?(idle_timeout_ms = Config.default.idle_timeout_ms)
  ?(write_timeout_ms = Config.default.write_timeout_ms)
  ?(buffer_size = Config.default.buffer_size)
  ?(liveview_secret = Config.default.liveview_secret)
  () ->
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

let tamper_last_char = fun value ->
  let len = String.length value in
  let prefix = String.sub value ~offset:0 ~len:(len - 1) in
  let last = String.get_unchecked value ~at:(len - 1) in
  let replacement =
    if last = 'A' then
      "B"
    else
      "A"
  in
  prefix ^ replacement

let test_http1_websocket_accept_matches_rfc_example = fun _ctx ->
  Test.assert_equal
    ~expected:"s3pPLMBiTxaQ9kYGzzhZRbK+xOo="
    ~actual:(Http1.compute_websocket_accept valid_websocket_key);
  Ok ()

let test_http1_websocket_upgrade_accepts_valid_request = fun _ctx ->
  match Http1.validate_websocket_upgrade (websocket_request ()) with
  | Ok key ->
      Test.assert_equal ~expected:valid_websocket_key ~actual:key;
      Ok ()
  | Error error -> Error (Http1.websocket_upgrade_error_to_string error)

let test_http1_websocket_upgrade_rejects_non_get = fun _ctx ->
  match Http1.validate_websocket_upgrade (websocket_request ~method_:Net.Http.Method.Post ()) with
  | Error (Http1.InvalidWebSocketMethod Net.Http.Method.Post) -> Ok ()
  | Ok _ -> Error "expected WebSocket upgrade to reject non-GET request"
  | Error error -> Error (Http1.websocket_upgrade_error_to_string error)

let test_http1_websocket_upgrade_rejects_http10 = fun _ctx ->
  match Http1.validate_websocket_upgrade (websocket_request ~version:Net.Http.Version.Http10 ()) with
  | Error (Http1.InvalidWebSocketVersion Net.Http.Version.Http10) -> Ok ()
  | Ok _ -> Error "expected WebSocket upgrade to reject HTTP/1.0 request"
  | Error error -> Error (Http1.websocket_upgrade_error_to_string error)

let test_http1_websocket_upgrade_requires_upgrade_header = fun _ctx ->
  let headers = [
    ("connection", "keep-alive, Upgrade");
    ("sec-websocket-version", "13");
    ("sec-websocket-key", valid_websocket_key);
  ]
  in
  match Http1.validate_websocket_upgrade (websocket_request ~headers ()) with
  | Error Http1.MissingWebSocketUpgrade -> Ok ()
  | Ok _ -> Error "expected WebSocket upgrade to require Upgrade header"
  | Error error -> Error (Http1.websocket_upgrade_error_to_string error)

let test_http1_websocket_upgrade_rejects_invalid_upgrade_header = fun _ctx ->
  let headers = [
    ("upgrade", "h2c");
    ("connection", "Upgrade");
    ("sec-websocket-version", "13");
    ("sec-websocket-key", valid_websocket_key);
  ]
  in
  match Http1.validate_websocket_upgrade (websocket_request ~headers ()) with
  | Error (Http1.InvalidWebSocketUpgrade { value = "h2c" }) -> Ok ()
  | Ok _ -> Error "expected WebSocket upgrade to reject non-websocket Upgrade header"
  | Error error -> Error (Http1.websocket_upgrade_error_to_string error)

let test_http1_websocket_upgrade_requires_connection_token = fun _ctx ->
  let headers = [
    ("upgrade", "websocket");
    ("connection", "keep-alive");
    ("sec-websocket-version", "13");
    ("sec-websocket-key", valid_websocket_key);
  ]
  in
  match Http1.validate_websocket_upgrade (websocket_request ~headers ()) with
  | Error Http1.MissingWebSocketConnectionUpgrade -> Ok ()
  | Ok _ -> Error "expected WebSocket upgrade to require Connection upgrade token"
  | Error error -> Error (Http1.websocket_upgrade_error_to_string error)

let test_http1_websocket_upgrade_requires_version_13 = fun _ctx ->
  let headers = [
    ("upgrade", "websocket");
    ("connection", "Upgrade");
    ("sec-websocket-version", "12");
    ("sec-websocket-key", valid_websocket_key);
  ]
  in
  match Http1.validate_websocket_upgrade (websocket_request ~headers ()) with
  | Error (Http1.UnsupportedWebSocketVersion { value = "12"; expected = "13" }) -> Ok ()
  | Ok _ -> Error "expected WebSocket upgrade to require Sec-WebSocket-Version: 13"
  | Error error -> Error (Http1.websocket_upgrade_error_to_string error)

let test_http1_websocket_upgrade_rejects_invalid_key = fun _ctx ->
  let headers = [
    ("upgrade", "websocket");
    ("connection", "Upgrade");
    ("sec-websocket-version", "13");
    ("sec-websocket-key", Encoding.Base64.encode "too-short");
  ]
  in
  match Http1.validate_websocket_upgrade (websocket_request ~headers ()) with
  | Error (
    Http1.InvalidWebSocketKey { reason = Http1.InvalidLength { actual = 9; expected = 16 }; _ }
  ) ->
      Ok ()
  | Ok _ -> Error "expected WebSocket upgrade to reject invalid Sec-WebSocket-Key"
  | Error error -> Error (Http1.websocket_upgrade_error_to_string error)

let test_http1_websocket_frame_limits_accept_small_frame = fun _ctx ->
  let frame = Http.Ws.Frame.text "abc" in
  Test.assert_equal
    ~expected:(Ok ())
    ~actual:(Http1.validate_websocket_frame_limits ~max_frame_size:3 ~max_message_size:3 frame);
  Ok ()

let test_http1_websocket_frame_limits_reject_oversized_frame = fun _ctx ->
  let frame = Http.Ws.Frame.text "abcd" in
  match Http1.validate_websocket_frame_limits ~max_frame_size:3 ~max_message_size:10 frame with
  | Error (Http1.WebSocketFrameTooLarge { size = 4; limit = 3 }) -> Ok ()
  | Ok () -> Error "expected oversized WebSocket frame to fail"
  | Error error -> Error (Http1.websocket_frame_limit_error_to_string error)

let test_http1_websocket_frame_limits_reject_oversized_message = fun _ctx ->
  let frame = Http.Ws.Frame.text "abcd" in
  match Http1.validate_websocket_frame_limits ~max_frame_size:10 ~max_message_size:3 frame with
  | Error (Http1.WebSocketMessageTooLarge { size = 4; limit = 3 }) -> Ok ()
  | Ok () -> Error "expected oversized WebSocket message to fail"
  | Error error -> Error (Http1.websocket_frame_limit_error_to_string error)

let test_http1_body_headers_accept_valid_content_length = fun _ctx ->
  let req = http_request ~headers:[ ("content-length", " 12 "); ] () in
  Test.assert_equal ~expected:(Ok 12) ~actual:(Http1.validate_request_body_headers req);
  Ok ()

let test_http1_body_headers_reject_invalid_content_length = fun _ctx ->
  let req = http_request ~headers:[ ("content-length", "abc"); ] () in
  match Http1.validate_request_body_headers req with
  | Error (Http1.InvalidContentLength { value = "abc"; reason = Http1.InvalidInteger }) -> Ok ()
  | Ok _ -> Error "expected invalid content-length to fail"
  | Error error -> Error (Http1.request_body_header_error_to_string error)

let test_http1_body_headers_reject_negative_content_length = fun _ctx ->
  let req = http_request ~headers:[ ("content-length", "-1"); ] () in
  match Http1.validate_request_body_headers req with
  | Error (Http1.InvalidContentLength { value = "-1"; reason = Http1.NegativeLength -1 }) -> Ok ()
  | Ok _ -> Error "expected negative content-length to fail"
  | Error error -> Error (Http1.request_body_header_error_to_string error)

let test_http1_body_headers_reject_conflicting_content_length = fun _ctx ->
  let req = http_request ~headers:[ ("content-length", "3"); ("content-length", "4"); ] () in
  match Http1.validate_request_body_headers req with
  | Error (Http1.ConflictingContentLength { values }) ->
      Test.assert_equal ~expected:2 ~actual:(List.length values);
      Test.assert_true (List.contains values ~value:"3");
      Test.assert_true (List.contains values ~value:"4");
      Ok ()
  | Ok _ -> Error "expected conflicting content-length headers to fail"
  | Error error -> Error (Http1.request_body_header_error_to_string error)

let test_http1_body_headers_allow_duplicate_matching_content_length = fun _ctx ->
  let req = http_request ~headers:[ ("content-length", "3"); ("content-length", "3"); ] () in
  Test.assert_equal ~expected:(Ok 3) ~actual:(Http1.validate_request_body_headers req);
  Ok ()

let test_http1_body_headers_reject_content_length_above_limit = fun _ctx ->
  let req = http_request ~headers:[ ("content-length", "1025"); ] () in
  match Http1.validate_request_body_headers ~max_body_size:1_024 req with
  | Error (Http1.ContentLengthExceedsLimit { length = 1_025; limit = 1_024 }) -> Ok ()
  | Ok _ -> Error "expected content-length above max body size to fail"
  | Error error -> Error (Http1.request_body_header_error_to_string error)

let test_http1_body_headers_reject_transfer_encoding_with_content_length = fun _ctx ->
  let req =
    http_request ~headers:[ ("content-length", "3"); ("transfer-encoding", "chunked"); ] ()
  in
  match Http1.validate_request_body_headers req with
  | Error (Http1.TransferEncodingWithContentLength { transfer_encoding; content_lengths }) ->
      Test.assert_equal ~expected:"chunked" ~actual:transfer_encoding;
      Test.assert_equal ~expected:[ "3"; ] ~actual:content_lengths;
      Ok ()
  | Ok _ -> Error "expected transfer-encoding with content-length to fail"
  | Error error -> Error (Http1.request_body_header_error_to_string error)

let test_http1_body_headers_reject_chunked_transfer_encoding = fun _ctx ->
  let req = http_request ~headers:[ ("transfer-encoding", "chunked"); ] () in
  match Http1.validate_request_body_headers req with
  | Error (Http1.UnsupportedTransferEncoding { value = "chunked" }) -> Ok ()
  | Ok _ -> Error "expected unsupported transfer-encoding to fail"
  | Error error -> Error (Http1.request_body_header_error_to_string error)

let test_http1_body_split_preserves_pipelined_bytes = fun _ctx ->
  let (body, remaining) = Http1.split_request_body "abcGET /next HTTP/1.1\r\n\r\n" 3 in
  Test.assert_equal ~expected:"abc" ~actual:body;
  Test.assert_equal ~expected:"GET /next HTTP/1.1\r\n\r\n" ~actual:remaining;
  Ok ()

let test_http1_body_split_keeps_zero_length_body_empty = fun _ctx ->
  let (body, remaining) = Http1.split_request_body "GET /next HTTP/1.1\r\n\r\n" 0 in
  Test.assert_equal ~expected:"" ~actual:body;
  Test.assert_equal ~expected:"GET /next HTTP/1.1\r\n\r\n" ~actual:remaining;
  Ok ()

let test_http1_wraps_known_upstream_parse_errors = fun _ctx ->
  Test.assert_equal
    ~expected:(Http1.UpstreamParseError (Http.Http1.Common.RequestLineTooLong { max_length = 8_192 }))
    ~actual:(Http1.parse_error_of_upstream_error
      (Http.Http1.Common.RequestLineTooLong { max_length = 8_192 }));
  Test.assert_equal
    ~expected:(Http1.UpstreamParseError (Http.Http1.Common.InvalidHeaderFormat Http.Http1.Common.MissingColon))
    ~actual:(Http1.parse_error_of_upstream_error
      (Http.Http1.Common.InvalidHeaderFormat Http.Http1.Common.MissingColon));
  Test.assert_equal
    ~expected:(Http1.UpstreamParseError (Http.Http1.Common.HeaderTooLong { max_length = 8_192 }))
    ~actual:(Http1.parse_error_of_upstream_error
      (Http.Http1.Common.HeaderTooLong { max_length = 8_192 }));
  Ok ()

let test_http1_preserves_upstream_parse_error_payload = fun _ctx ->
  match Http1.parse_error_of_upstream_error Http.Http1.Common.InvalidHttpVersion with
  | Http1.UpstreamParseError Http.Http1.Common.InvalidHttpVersion -> Ok ()
  | _ -> Error "expected upstream parser error payload to be preserved"

let test_http1_request_headers_reject_missing_host = fun _ctx ->
  let req = http_request () in
  match Http1.validate_request_headers req with
  | Error Http1.MissingHostHeader -> Ok ()
  | Ok () -> Error "expected HTTP/1.1 request without Host to fail"

let test_http1_request_headers_accept_host = fun _ctx ->
  let req = http_request ~headers:[ ("host", "example.com"); ] () in
  Test.assert_equal ~expected:(Ok ()) ~actual:(Http1.validate_request_headers req);
  Ok ()

let test_http1_request_headers_allow_http10_without_host = fun _ctx ->
  let req = http_request ~version:Net.Http.Version.Http10 () in
  Test.assert_equal ~expected:(Ok ()) ~actual:(Http1.validate_request_headers req);
  Ok ()

let test_http1_keep_alive_defaults_for_http11 = fun _ctx ->
  let req =
    http_request ~version:Net.Http.Version.Http11 ()
    |> Suri.Request.from_http ~body:""
  in
  Test.assert_true (Http1.should_keep_alive req);
  Ok ()

let test_http1_keep_alive_parses_close_token_case_insensitively = fun _ctx ->
  let req =
    http_request
      ~version:Net.Http.Version.Http11
      ~headers:[ ("connection", "keep-alive, CLOSE"); ]
      ()
    |> Suri.Request.from_http ~body:""
  in
  Test.assert_false (Http1.should_keep_alive req);
  Ok ()

let test_http1_keep_alive_parses_http10_keep_alive_token = fun _ctx ->
  let req =
    http_request
      ~version:Net.Http.Version.Http10
      ~headers:[ ("connection", "Upgrade, Keep-Alive"); ]
      ()
    |> Suri.Request.from_http ~body:""
  in
  Test.assert_true (Http1.should_keep_alive req);
  Ok ()

let test_http1_keep_alive_limit_allows_before_final_request = fun _ctx ->
  let req =
    http_request ~version:Net.Http.Version.Http11 ()
    |> Suri.Request.from_http ~body:""
  in
  Test.assert_true
    (Http1.should_continue_keep_alive ~max_keep_alive_requests:2 ~requests_processed:1 req);
  Ok ()

let test_http1_keep_alive_limit_closes_after_final_request = fun _ctx ->
  let req =
    http_request ~version:Net.Http.Version.Http11 ()
    |> Suri.Request.from_http ~body:""
  in
  Test.assert_false
    (Http1.should_continue_keep_alive ~max_keep_alive_requests:2 ~requests_processed:2 req);
  Ok ()

let test_http1_response_rejects_invalid_header_name = fun _ctx ->
  let res = Response.ok ~headers:[ ("bad name", "value"); ] ~body:"ok" () in
  match Http1.serialize_response res with
  | Error (Http1.InvalidHeaderName { name; reason = Http1.InvalidHeaderNameChar { char; index } }) ->
      Test.assert_equal ~expected:"bad name" ~actual:name;
      Test.assert_equal ~expected:' ' ~actual:char;
      Test.assert_equal ~expected:3 ~actual:index;
      Ok ()
  | _ ->
      Test.assert_true false;
      Ok ()

let test_http1_response_rejects_empty_header_name = fun _ctx ->
  let res = Response.ok ~headers:[ ("", "value"); ] ~body:"ok" () in
  match Http1.serialize_response res with
  | Error (Http1.InvalidHeaderName { name; reason = Http1.EmptyHeaderName }) ->
      Test.assert_equal ~expected:"" ~actual:name;
      Ok ()
  | _ ->
      Test.assert_true false;
      Ok ()

let test_http1_response_rejects_header_injection = fun _ctx ->
  let res = Response.ok ~headers:[ ("x-test", "ok\r\nx-evil: yes"); ] ~body:"ok" () in
  match Http1.serialize_response res with
  | Error (
    Http1.InvalidHeaderValue { name; value = _; reason = Http1.InvalidHeaderValueChar { char; index } }
  ) ->
      Test.assert_equal ~expected:"x-test" ~actual:name;
      Test.assert_equal ~expected:'\r' ~actual:char;
      Test.assert_equal ~expected:2 ~actual:index;
      Ok ()
  | _ ->
      Test.assert_true false;
      Ok ()

let test_http1_response_rejects_header_control_values = fun _ctx ->
  let res = Response.ok ~headers:[ ("x-test", "bad\001value"); ] ~body:"ok" () in
  match Http1.serialize_response res with
  | Error (
    Http1.InvalidHeaderValue { name; value = _; reason = Http1.InvalidHeaderValueChar { char; index } }
  ) ->
      Test.assert_equal ~expected:"x-test" ~actual:name;
      Test.assert_equal ~expected:'\001' ~actual:char;
      Test.assert_equal ~expected:3 ~actual:index;
      Ok ()
  | _ ->
      Test.assert_true false;
      Ok ()

let test_http1_response_omits_body_for_no_content = fun _ctx ->
  let res = Response.no_content ~headers:[ ("content-length", "7"); ] ~body:"ignored" () in
  match Http1.serialize_response res with
  | Ok bytes ->
      Test.assert_false (String.contains bytes "content-length");
      Test.assert_false (String.contains bytes "ignored");
      Ok ()
  | Error _ ->
      Test.assert_true false;
      Ok ()

let test_http1_response_omits_body_for_not_modified = fun _ctx ->
  let res = Response.not_modified ~headers:[ ("content-length", "7"); ] ~body:"ignored" () in
  match Http1.serialize_response res with
  | Ok bytes ->
      Test.assert_false (String.contains bytes "content-length");
      Test.assert_false (String.contains bytes "ignored");
      Ok ()
  | Error _ ->
      Test.assert_true false;
      Ok ()

let test_http1_response_omits_body_for_informational_status = fun _ctx ->
  let res = Response.continue ~headers:[ ("content-length", "7"); ] ~body:"ignored" () in
  match Http1.serialize_response res with
  | Ok bytes ->
      Test.assert_false (String.contains bytes "content-length");
      Test.assert_false (String.contains bytes "ignored");
      Ok ()
  | Error _ ->
      Test.assert_true false;
      Ok ()

let test_http1_response_sets_content_length_for_body = fun _ctx ->
  let res = Response.ok ~body:"hello" () in
  match Http1.serialize_response res with
  | Ok bytes ->
      Test.assert_true (String.contains bytes "content-length: 5");
      Test.assert_true (String.contains bytes "\r\n\r\nhello");
      Ok ()
  | Error _ ->
      Test.assert_true false;
      Ok ()

let test_http1_response_does_not_add_vary_without_compression = fun _ctx ->
  let res = Response.ok ~body:"hello" () in
  match Http1.serialize_response res with
  | Ok bytes ->
      Test.assert_false (String.contains bytes "vary: accept-encoding");
      Test.assert_false (String.contains bytes "Vary: accept-encoding");
      Ok ()
  | Error _ ->
      Test.assert_true false;
      Ok ()

let tests =
  Test.[
    case
      "http1 websocket accept matches rfc example"
      test_http1_websocket_accept_matches_rfc_example;
    case
      "http1 websocket upgrade accepts valid request"
      test_http1_websocket_upgrade_accepts_valid_request;
    case "http1 websocket upgrade rejects non get" test_http1_websocket_upgrade_rejects_non_get;
    case "http1 websocket upgrade rejects http10" test_http1_websocket_upgrade_rejects_http10;
    case
      "http1 websocket upgrade requires upgrade header"
      test_http1_websocket_upgrade_requires_upgrade_header;
    case
      "http1 websocket upgrade rejects invalid upgrade header"
      test_http1_websocket_upgrade_rejects_invalid_upgrade_header;
    case
      "http1 websocket upgrade requires connection token"
      test_http1_websocket_upgrade_requires_connection_token;
    case
      "http1 websocket upgrade requires version 13"
      test_http1_websocket_upgrade_requires_version_13;
    case
      "http1 websocket upgrade rejects invalid key"
      test_http1_websocket_upgrade_rejects_invalid_key;
    case
      "http1 websocket frame limits accept small frame"
      test_http1_websocket_frame_limits_accept_small_frame;
    case
      "http1 websocket frame limits reject oversized frame"
      test_http1_websocket_frame_limits_reject_oversized_frame;
    case
      "http1 websocket frame limits reject oversized message"
      test_http1_websocket_frame_limits_reject_oversized_message;
    case
      "http1 body headers accept valid content length"
      test_http1_body_headers_accept_valid_content_length;
    case
      "http1 body headers reject invalid content length"
      test_http1_body_headers_reject_invalid_content_length;
    case
      "http1 body headers reject negative content length"
      test_http1_body_headers_reject_negative_content_length;
    case
      "http1 body headers reject conflicting content length"
      test_http1_body_headers_reject_conflicting_content_length;
    case
      "http1 body headers allow duplicate matching content length"
      test_http1_body_headers_allow_duplicate_matching_content_length;
    case
      "http1 body headers reject content length above limit"
      test_http1_body_headers_reject_content_length_above_limit;
    case
      "http1 body headers reject transfer encoding with content length"
      test_http1_body_headers_reject_transfer_encoding_with_content_length;
    case
      "http1 body headers reject chunked transfer encoding"
      test_http1_body_headers_reject_chunked_transfer_encoding;
    case
      "http1 body split preserves pipelined bytes"
      test_http1_body_split_preserves_pipelined_bytes;
    case
      "http1 body split keeps zero length body empty"
      test_http1_body_split_keeps_zero_length_body_empty;
    case "http1 wraps known upstream parse errors" test_http1_wraps_known_upstream_parse_errors;
    case
      "http1 preserves upstream parse error payload"
      test_http1_preserves_upstream_parse_error_payload;
    case "http1 request headers reject missing host" test_http1_request_headers_reject_missing_host;
    case "http1 request headers accept host" test_http1_request_headers_accept_host;
    case
      "http1 request headers allow http10 without host"
      test_http1_request_headers_allow_http10_without_host;
    case "http1 keep alive defaults for http11" test_http1_keep_alive_defaults_for_http11;
    case
      "http1 keep alive parses close token case insensitively"
      test_http1_keep_alive_parses_close_token_case_insensitively;
    case
      "http1 keep alive parses http10 keep alive token"
      test_http1_keep_alive_parses_http10_keep_alive_token;
    case
      "http1 keep alive limit allows before final request"
      test_http1_keep_alive_limit_allows_before_final_request;
    case
      "http1 keep alive limit closes after final request"
      test_http1_keep_alive_limit_closes_after_final_request;
    case
      "http1 response rejects invalid header name"
      test_http1_response_rejects_invalid_header_name;
    case "http1 response rejects empty header name" test_http1_response_rejects_empty_header_name;
    case "http1 response rejects header injection" test_http1_response_rejects_header_injection;
    case
      "http1 response rejects header control values"
      test_http1_response_rejects_header_control_values;
    case "http1 response omits body for no content" test_http1_response_omits_body_for_no_content;
    case
      "http1 response omits body for not modified"
      test_http1_response_omits_body_for_not_modified;
    case
      "http1 response omits body for informational status"
      test_http1_response_omits_body_for_informational_status;
    case
      "http1 response sets content length for body"
      test_http1_response_sets_content_length_for_body;
    case
      "http1 response does not add vary without compression"
      test_http1_response_does_not_add_vary_without_compression;
  ]

let main ~args = Test.Cli.main ~name:"suri:web-server-http1" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
