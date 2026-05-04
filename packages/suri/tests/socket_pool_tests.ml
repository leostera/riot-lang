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
module SocketPool = Suri.Testing.Internal.SocketPool
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

let test_connection_write_all_retries_short_writes = fun _ctx ->
  let calls = ref [] in
  let write _buf ~pos ~len =
    calls := (pos, len) :: !calls;
    if len > 3 then
      Ok 3
    else
      Ok len
  in
  match Connection.write_all_with ~write "abcdefgh" with
  | Ok () ->
      Test.assert_equal ~expected:[ (6, 2); (3, 5); (0, 8); ] ~actual:!calls;
      Ok ()
  | Error Connection.Closed -> Error "expected short writes to complete"
  | Error _ -> Error "unexpected connection write error"

let test_connection_write_all_treats_zero_write_as_closed = fun _ctx ->
  let calls = ref 0 in
  let write _buf ~pos:_ ~len:_ =
    calls := !calls + 1;
    Ok 0
  in
  match Connection.write_all_with ~write "abc" with
  | Error Connection.Closed ->
      Test.assert_equal ~expected:1 ~actual:!calls;
      Ok ()
  | Ok () -> Error "expected zero-byte write to close connection"
  | Error _ -> Error "unexpected connection write error"

let test_connection_write_all_skips_empty_payload = fun _ctx ->
  let calls = ref 0 in
  let write _buf ~pos:_ ~len:_ =
    calls := !calls + 1;
    Ok 1
  in
  match Connection.write_all_with ~write "" with
  | Ok () ->
      Test.assert_equal ~expected:0 ~actual:!calls;
      Ok ()
  | Error Connection.Closed -> Error "expected empty payload to complete without writes"
  | Error _ -> Error "unexpected connection write error"

let test_connection_send_file_slice_extracts_range = fun _ctx ->
  match Connection.send_file_slice ~off:2 ~len:4 "abcdefgh" with
  | Ok chunk ->
      Test.assert_equal ~expected:"cdef" ~actual:chunk;
      Ok ()
  | Error (Connection.InvalidRange _) -> Error "expected valid send_file range"
  | Error _ -> Error "unexpected send_file error"

let test_connection_send_file_slice_allows_zero_length = fun _ctx ->
  match Connection.send_file_slice ~off:8 ~len:0 "abcdefgh" with
  | Ok chunk ->
      Test.assert_equal ~expected:"" ~actual:chunk;
      Ok ()
  | Error (Connection.InvalidRange _) -> Error "expected zero-length send_file range"
  | Error _ -> Error "unexpected send_file error"

let test_connection_send_file_slice_rejects_invalid_range = fun _ctx ->
  match Connection.send_file_slice ~off:6 ~len:3 "abcdefgh" with
  | Error (Connection.InvalidRange { off; len; size }) ->
      Test.assert_equal ~expected:6 ~actual:off;
      Test.assert_equal ~expected:3 ~actual:len;
      Test.assert_equal ~expected:8 ~actual:size;
      Ok ()
  | Ok _ -> Error "expected send_file range beyond file size to fail"
  | Error _ -> Error "unexpected send_file error"

let test_socket_pool_rejects_invalid_acceptors = fun _ctx ->
  match SocketPool.validate_start_options ~acceptors:0 ~buffer_size:4_096 with
  | Error (SocketPool.InvalidAcceptors 0) -> Ok ()
  | Ok () -> Error "expected invalid acceptor count"
  | Error _ -> Error "expected invalid acceptor count"

let test_socket_pool_rejects_invalid_buffer_size = fun _ctx ->
  match SocketPool.validate_start_options ~acceptors:1 ~buffer_size:0 with
  | Error (SocketPool.InvalidBufferSize 0) -> Ok ()
  | Ok () -> Error "expected invalid buffer size"
  | Error _ -> Error "expected invalid buffer size"

let tests =
  Test.[
    case "connection write all retries short writes" test_connection_write_all_retries_short_writes;
    case
      "connection write all treats zero write as closed"
      test_connection_write_all_treats_zero_write_as_closed;
    case "connection write all skips empty payload" test_connection_write_all_skips_empty_payload;
    case "connection send file slice extracts range" test_connection_send_file_slice_extracts_range;
    case
      "connection send file slice allows zero length"
      test_connection_send_file_slice_allows_zero_length;
    case
      "connection send file slice rejects invalid range"
      test_connection_send_file_slice_rejects_invalid_range;
    case "socket pool rejects invalid acceptors" test_socket_pool_rejects_invalid_acceptors;
    case "socket pool rejects invalid buffer size" test_socket_pool_rejects_invalid_buffer_size;
  ]

let main ~args = Test.Cli.main ~name:"suri:socket-pool" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
