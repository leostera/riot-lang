open Std

(** HTTP/2 frame serialization tests *)
module Frame = Http.Http2.Frame
module Serializer = Http.Http2.Serializer
module Connection = Http.Http2.Connection
module Parser = Http.Http2.Parser

let frame_payload_length_at = fun serialized ~offset ->
  let byte index = Char.code (String.get_unchecked serialized ~at:(offset + index)) in
  (byte 0 lsl 16) lor (byte 1 lsl 8) lor byte 2

let frame_payload_length = fun serialized -> frame_payload_length_at serialized ~offset:0

let settings_frame_with_payload = fun payload -> "\x00\x00\x06\x04\x00\x00\x00\x00\x00" ^ payload

let expect_parse_error = fun bytes expected ->
  match Parser.parse_frame bytes with
  | Parser.Error err when String.equal err expected -> Result.Ok ()
  | Parser.Error err -> Result.Error ("Wrong parse error: " ^ err)
  | Parser.Need_more -> Result.Error ("Expected parse error: " ^ expected)
  | Parser.Done _ -> Result.Error ("Expected parse error, but frame parsed: " ^ expected)

let test_serialize_settings_frame = fun _ctx ->
  let frame = {
    Frame.length = 0;
    frame_type = Frame.Settings;
    flags =
      {
        Frame.end_stream = false;
        end_headers = false;
        padded = false;
        priority = false;
        ack = false;
      };
    stream_id = 0;
    payload = Frame.SettingsPayload [];
  }
  in
  let serialized = Serializer.serialize_frame frame in
  if String.length serialized >= 9 then
    Result.Ok ()
  else
    Result.Error ("Serialized frame too short: " ^ Int.to_string (String.length serialized))

let test_serialize_data_frame = fun _ctx ->
  let frame = {
    Frame.length = 5;
    frame_type = Frame.Data;
    flags =
      {
        Frame.end_stream = true;
        end_headers = false;
        padded = false;
        priority = false;
        ack = false;
      };
    stream_id = 1;
    payload = Frame.DataPayload { data = "hello"; pad_length = None };
  }
  in
  let serialized = Serializer.serialize_frame frame in
  if String.length serialized > 9 then
    Result.Ok ()
  else
    Result.Error "Serialized data frame has no payload"

let test_serialize_recomputes_payload_length = fun _ctx ->
  let frame = {
    Frame.length = 0;
    frame_type = Frame.Data;
    flags =
      {
        Frame.end_stream = true;
        end_headers = false;
        padded = false;
        priority = false;
        ack = false;
      };
    stream_id = 1;
    payload = Frame.DataPayload { data = "hello"; pad_length = None };
  }
  in
  let serialized = Serializer.serialize_frame frame in
  let length = frame_payload_length serialized in
  if Int.equal length 5 then
    Result.Ok ()
  else
    Result.Error ("Serialized frame length should be 5, got " ^ Int.to_string length)

let test_serialize_settings_payload_length = fun _ctx ->
  let frame = {
    Frame.length = 0;
    frame_type = Frame.Settings;
    flags =
      {
        Frame.end_stream = false;
        end_headers = false;
        padded = false;
        priority = false;
        ack = false;
      };
    stream_id = 0;
    payload = Frame.SettingsPayload [ Frame.HeaderTableSize 4_096; ];
  }
  in
  let serialized = Serializer.serialize_frame frame in
  let length = frame_payload_length serialized in
  if Int.equal length 6 then
    Result.Ok ()
  else
    Result.Error ("Serialized settings length should be 6, got " ^ Int.to_string length)

let test_serialize_settings_ack_has_empty_payload = fun _ctx ->
  let frame = {
    Frame.length = 0;
    frame_type = Frame.Settings;
    flags =
      {
        Frame.end_stream = false;
        end_headers = false;
        padded = false;
        priority = false;
        ack = true;
      };
    stream_id = 0;
    payload = Frame.SettingsPayload [ Frame.HeaderTableSize 4_096; ];
  }
  in
  let serialized = Serializer.serialize_frame frame in
  let length = frame_payload_length serialized in
  if Int.equal length 0 && Int.equal (String.length serialized) 9 then
    Result.Ok ()
  else
    Result.Error ("Serialized settings ack should have empty payload, got length "
    ^ Int.to_string length)

let test_client_preface_settings_payload_length = fun _ctx ->
  let conn = Connection.create ~role:Connection.Client () in
  let preface = Connection.send_preface conn in
  let length = frame_payload_length_at preface ~offset:24 in
  if Int.equal length 30 then
    Result.Ok ()
  else
    Result.Error ("Client preface settings length should be 30, got " ^ Int.to_string length)

let test_server_preface_settings_payload_length = fun _ctx ->
  let conn = Connection.create ~role:Connection.Server () in
  let preface = Connection.send_preface conn in
  let length = frame_payload_length preface in
  if Int.equal length 30 then
    Result.Ok ()
  else
    Result.Error ("Server preface settings length should be 30, got " ^ Int.to_string length)

let test_process_data_buffers_split_frame_header = fun _ctx ->
  let conn = Connection.create ~role:Connection.Server () in
  let frame = Frame.settings [ Frame.HeaderTableSize 1_024; ] in
  let bytes = Serializer.serialize_frame frame in
  let first = String.sub bytes ~offset:0 ~len:4 in
  let rest = String.sub bytes ~offset:4 ~len:(String.length bytes - 4) in
  match Connection.process_data conn (Std.IO.Bytes.from_string first) with
  | Error err -> Result.Error err
  | Ok events when List.length events != 0 ->
      Result.Error "split frame header should not emit events before the frame is complete"
  | Ok _ -> (
      match Connection.process_data conn (Std.IO.Bytes.from_string rest) with
      | Ok [ Connection.SettingsReceived [ Frame.HeaderTableSize size ] ] when Int.equal size 1_024 ->
          Result.Ok ()
      | Ok _ -> Result.Error "split frame header did not emit the expected settings event"
      | Error err -> Result.Error err
    )

let test_process_data_buffers_split_frame_payload = fun _ctx ->
  let conn = Connection.create ~role:Connection.Server () in
  let frame = Frame.settings [ Frame.HeaderTableSize 1_024; Frame.MaxFrameSize 16_384; ] in
  let bytes = Serializer.serialize_frame frame in
  let first = String.sub bytes ~offset:0 ~len:12 in
  let rest = String.sub bytes ~offset:12 ~len:(String.length bytes - 12) in
  match Connection.process_data conn (Std.IO.Bytes.from_string first) with
  | Error err -> Result.Error err
  | Ok events when List.length events != 0 ->
      Result.Error "split frame payload should not emit events before the frame is complete"
  | Ok _ -> (
      match Connection.process_data conn (Std.IO.Bytes.from_string rest) with
      | Ok [ Connection.SettingsReceived [ Frame.HeaderTableSize table_size; Frame.MaxFrameSize frame_size ] ] when Int.equal
        table_size
        1_024
      && Int.equal frame_size 16_384 -> Result.Ok ()
      | Ok _ -> Result.Error "split frame payload did not emit the expected settings event"
      | Error err -> Result.Error err
    )

let test_frame_types = fun _ctx ->
  let types = [ Frame.Data; Frame.Headers; Frame.Settings; Frame.Ping; Frame.Goaway; ] in
  if List.length types = 5 then
    Result.Ok ()
  else
    Result.Error "Frame types count mismatch"

let test_parse_settings_rejects_invalid_enable_push = fun _ctx ->
  let bytes = settings_frame_with_payload "\x00\x02\x00\x00\x00\x02" in
  match Parser.parse_frame bytes with
  | Parser.Error "SETTINGS_ENABLE_PUSH must be 0 or 1" -> Result.Ok ()
  | Parser.Error err -> Result.Error ("Wrong parse error: " ^ err)
  | Parser.Need_more -> Result.Error "Expected invalid SETTINGS_ENABLE_PUSH to fail"
  | Parser.Done _ -> Result.Error "Invalid SETTINGS_ENABLE_PUSH was accepted"

let test_parse_settings_rejects_initial_window_overflow = fun _ctx ->
  let bytes = settings_frame_with_payload "\x00\x04\x80\x00\x00\x00" in
  match Parser.parse_frame bytes with
  | Parser.Error "SETTINGS_INITIAL_WINDOW_SIZE must be at most 2147483647" -> Result.Ok ()
  | Parser.Error err -> Result.Error ("Wrong parse error: " ^ err)
  | Parser.Need_more -> Result.Error "Expected oversized SETTINGS_INITIAL_WINDOW_SIZE to fail"
  | Parser.Done _ -> Result.Error "Oversized SETTINGS_INITIAL_WINDOW_SIZE was accepted"

let test_parse_settings_rejects_small_max_frame_size = fun _ctx ->
  let bytes = settings_frame_with_payload "\x00\x05\x00\x00\x3f\xff" in
  match Parser.parse_frame bytes with
  | Parser.Error "SETTINGS_MAX_FRAME_SIZE must be between 16384 and 16777215" -> Result.Ok ()
  | Parser.Error err -> Result.Error ("Wrong parse error: " ^ err)
  | Parser.Need_more -> Result.Error "Expected small SETTINGS_MAX_FRAME_SIZE to fail"
  | Parser.Done _ -> Result.Error "Small SETTINGS_MAX_FRAME_SIZE was accepted"

let test_parse_settings_rejects_large_max_frame_size = fun _ctx ->
  let bytes = settings_frame_with_payload "\x00\x05\x01\x00\x00\x00" in
  match Parser.parse_frame bytes with
  | Parser.Error "SETTINGS_MAX_FRAME_SIZE must be between 16384 and 16777215" -> Result.Ok ()
  | Parser.Error err -> Result.Error ("Wrong parse error: " ^ err)
  | Parser.Need_more -> Result.Error "Expected large SETTINGS_MAX_FRAME_SIZE to fail"
  | Parser.Done _ -> Result.Error "Large SETTINGS_MAX_FRAME_SIZE was accepted"

let test_parse_data_rejects_stream_zero = fun _ctx ->
  expect_parse_error
    "\x00\x00\x00\x00\x00\x00\x00\x00\x00"
    "DATA frame must use a non-zero stream ID"

let test_parse_headers_rejects_stream_zero = fun _ctx ->
  expect_parse_error
    "\x00\x00\x00\x01\x00\x00\x00\x00\x00"
    "HEADERS frame must use a non-zero stream ID"

let test_parse_settings_rejects_nonzero_stream = fun _ctx ->
  expect_parse_error
    "\x00\x00\x00\x04\x00\x00\x00\x00\x01"
    "SETTINGS frame must use stream ID 0"

let test_parse_ping_rejects_nonzero_stream = fun _ctx ->
  expect_parse_error
    "\x00\x00\x08\x06\x00\x00\x00\x00\x01abcdefgh"
    "PING frame must use stream ID 0"

let test_parse_goaway_rejects_nonzero_stream = fun _ctx ->
  expect_parse_error
    "\x00\x00\x08\x07\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00"
    "GOAWAY frame must use stream ID 0"

let test_parse_window_update_allows_stream_zero = fun _ctx ->
  match Parser.parse_frame "\x00\x00\x04\x08\x00\x00\x00\x00\x00\x00\x00\x00\x01" with
  | Parser.Done { value = { Frame.frame_type = Frame.WindowUpdate; stream_id = 0; _ }; _ } ->
      Result.Ok ()
  | Parser.Done _ -> Result.Error "WINDOW_UPDATE stream 0 parsed as the wrong frame"
  | Parser.Need_more -> Result.Error "WINDOW_UPDATE stream 0 unexpectedly needed more data"
  | Parser.Error err -> Result.Error ("WINDOW_UPDATE stream 0 was rejected: " ^ err)

let test_parse_unknown_frame_preserves_payload = fun _ctx ->
  match Parser.parse_frame "\x00\x00\x03\x0b\x00\x00\x00\x00\x00abc" with
  | Parser.Done { value = { Frame.frame_type = Frame.Unknown 0x0b; stream_id = 0; payload = Frame.UnknownPayload "abc"; _ }; remaining = "" } ->
      Result.Ok ()
  | Parser.Done _ -> Result.Error "Unknown frame parsed with the wrong payload"
  | Parser.Need_more -> Result.Error "Unknown frame unexpectedly needed more data"
  | Parser.Error err -> Result.Error ("Unknown frame was rejected: " ^ err)

let test_process_data_ignores_unknown_frame = fun _ctx ->
  let conn = Connection.create ~role:Connection.Server () in
  match Connection.process_data
    conn
    (Std.IO.Bytes.from_string "\x00\x00\x03\x0b\x00\x00\x00\x00\x00abc") with
  | Ok [] -> Result.Ok ()
  | Ok _ -> Result.Error "Unknown frame emitted connection events"
  | Error err -> Result.Error ("Unknown frame failed connection processing: " ^ err)

let tests =
  Test.[
    case "serialize_settings_frame" test_serialize_settings_frame;
    case "serialize_data_frame" test_serialize_data_frame;
    case "serialize_recomputes_payload_length" test_serialize_recomputes_payload_length;
    case "serialize_settings_payload_length" test_serialize_settings_payload_length;
    case "serialize_settings_ack_has_empty_payload" test_serialize_settings_ack_has_empty_payload;
    case "client_preface_settings_payload_length" test_client_preface_settings_payload_length;
    case "server_preface_settings_payload_length" test_server_preface_settings_payload_length;
    case "process_data_buffers_split_frame_header" test_process_data_buffers_split_frame_header;
    case "process_data_buffers_split_frame_payload" test_process_data_buffers_split_frame_payload;
    case "frame_types" test_frame_types;
    case
      "parse_settings_rejects_invalid_enable_push"
      test_parse_settings_rejects_invalid_enable_push;
    case
      "parse_settings_rejects_initial_window_overflow"
      test_parse_settings_rejects_initial_window_overflow;
    case
      "parse_settings_rejects_small_max_frame_size"
      test_parse_settings_rejects_small_max_frame_size;
    case
      "parse_settings_rejects_large_max_frame_size"
      test_parse_settings_rejects_large_max_frame_size;
    case "parse_data_rejects_stream_zero" test_parse_data_rejects_stream_zero;
    case "parse_headers_rejects_stream_zero" test_parse_headers_rejects_stream_zero;
    case "parse_settings_rejects_nonzero_stream" test_parse_settings_rejects_nonzero_stream;
    case "parse_ping_rejects_nonzero_stream" test_parse_ping_rejects_nonzero_stream;
    case "parse_goaway_rejects_nonzero_stream" test_parse_goaway_rejects_nonzero_stream;
    case "parse_window_update_allows_stream_zero" test_parse_window_update_allows_stream_zero;
    case "parse_unknown_frame_preserves_payload" test_parse_unknown_frame_preserves_payload;
    case "process_data_ignores_unknown_frame" test_process_data_ignores_unknown_frame;
  ]

let main ~args:_ = Test.Cli.main ~name:"http:http2_parser" ~tests ~args:Env.args ()

let () = Runtime.run ~main ~args:Env.args ()
