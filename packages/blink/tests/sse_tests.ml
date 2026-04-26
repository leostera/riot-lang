open Std

module S = Blink.SSE

let test_single_data_event = fun _ctx ->
  match S.parse_event "data: hello\n\nrest" with
  | Some (S.Event event, remaining) ->
      Test.assert_equal ~expected:"hello" ~actual:event.data;
      Test.assert_equal ~expected:None ~actual:event.event_type;
      Test.assert_equal ~expected:None ~actual:event.id;
      Test.assert_equal ~expected:"rest" ~actual:remaining;
      Ok ()
  | _ -> Error "expected SSE data event"

let test_multiline_data_event = fun _ctx ->
  match S.parse_event "data: hello\ndata: world\n\n" with
  | Some (S.Event event, remaining) ->
      Test.assert_equal ~expected:"hello\nworld" ~actual:event.data;
      Test.assert_equal ~expected:"" ~actual:remaining;
      Ok ()
  | _ -> Error "expected multiline SSE data event"

let test_event_type_and_id_without_space = fun _ctx ->
  match S.parse_event "event:update\nid:42\ndata:payload\n\n" with
  | Some (S.Event event, _) ->
      Test.assert_equal ~expected:"payload" ~actual:event.data;
      Test.assert_equal ~expected:(Some "update") ~actual:event.event_type;
      Test.assert_equal ~expected:(Some "42") ~actual:event.id;
      Ok ()
  | _ -> Error "expected typed SSE event"

let test_comment_lines_are_ignored = fun _ctx ->
  match S.parse_event ": keepalive\ndata: hello\n\n" with
  | Some (S.Event event, _) ->
      Test.assert_equal ~expected:"hello" ~actual:event.data;
      Ok ()
  | _ -> Error "expected SSE event after comment"

let test_empty_frame_is_skipped = fun _ctx ->
  match S.parse_event "\n\ndata: next\n\n" with
  | Some (S.Skip, remaining) ->
      Test.assert_equal ~expected:"data: next\n\n" ~actual:remaining;
      Ok ()
  | _ -> Error "expected empty SSE frame to be skipped"

let test_done_marker_stops_stream = fun _ctx ->
  match S.parse_event "data: [DONE]\n\nignored" with
  | Some (S.Done, remaining) ->
      Test.assert_equal ~expected:"ignored" ~actual:remaining;
      Ok ()
  | _ -> Error "expected SSE done marker"

let test_crlf_delimiter = fun _ctx ->
  match S.parse_event "event: message\r\ndata: hello\r\n\r\n" with
  | Some (S.Event event, remaining) ->
      Test.assert_equal ~expected:"hello" ~actual:event.data;
      Test.assert_equal ~expected:(Some "message") ~actual:event.event_type;
      Test.assert_equal ~expected:"" ~actual:remaining;
      Ok ()
  | _ -> Error "expected CRLF-delimited SSE event"

let test_incomplete_buffer_returns_none = fun _ctx ->
  Test.assert_equal ~expected:None ~actual:(S.parse_event "data: partial");
  Ok ()

let tests =
  Test.[
    case "single data event" test_single_data_event;
    case "multiline data event" test_multiline_data_event;
    case "event type and id without space" test_event_type_and_id_without_space;
    case "comment lines are ignored" test_comment_lines_are_ignored;
    case "empty frame is skipped" test_empty_frame_is_skipped;
    case "done marker stops stream" test_done_marker_stops_stream;
    case "crlf delimiter" test_crlf_delimiter;
    case "incomplete buffer returns none" test_incomplete_buffer_returns_none;
  ]

let main ~args = Test.Cli.main ~name:"blink_sse_tests" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
