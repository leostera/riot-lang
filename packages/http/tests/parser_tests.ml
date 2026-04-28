open Std

(** HTTP/2 frame serialization tests *)
module Frame = Http.Http2.Frame
module Serializer = Http.Http2.Serializer
module Connection = Http.Http2.Connection
module Parser = Http.Http2.Parser
module ParserReader = Http.Http2.Parser_reader
module Hpack = Http.Http2.Hpack

let serialize_frame = fun frame ->
  match Serializer.serialize_frame frame with
  | Ok bytes -> bytes
  | Error error -> panic ("serialize_frame failed: " ^ Serializer.error_to_string error)

let send_preface = fun conn ->
  match Connection.send_preface conn with
  | Ok bytes -> bytes
  | Error error -> panic ("send_preface failed: " ^ Connection.error_to_string error)

let frame_payload_length_at = fun serialized ~offset ->
  let byte index = Char.code (String.get_unchecked serialized ~at:(offset + index)) in
  (byte 0 lsl 16) lor (byte 1 lsl 8) lor byte 2

let frame_payload_length = fun serialized -> frame_payload_length_at serialized ~offset:0

let settings_frame_with_payload = fun payload -> "\x00\x00\x06\x04\x00\x00\x00\x00\x00" ^ payload

let encode_header_block = fun headers ->
  let encoder = Hpack.create_encoder () in
  match Hpack.encode encoder ~sensitive_headers:[] () ~headers with
  | Ok bytes -> Std.IO.Bytes.to_string bytes
  | Error error -> panic ("hpack encode failed: " ^ Hpack.encode_error_to_string error)

let expect_parse_error = fun bytes expected ->
  match Parser.parse_frame bytes with
  | Parser.Error err when err = expected -> Result.Ok ()
  | Parser.Error err -> Result.Error ("Wrong parse error: " ^ Parser.error_to_string err)
  | Parser.Need_more -> Result.Error "Expected parse error"
  | Parser.Done _ -> Result.Error "Expected parse error, but frame parsed"

let expect_reader_parse_error = fun bytes expected ->
  let parser = ParserReader.create () in
  let reader = Std.IO.Reader.from_string bytes in
  match ParserReader.parse parser reader with
  | ParserReader.Error (ParserReader.FrameParseFailed err) when err = expected -> Result.Ok ()
  | ParserReader.Error error ->
      Result.Error ("Wrong reader parse error: " ^ ParserReader.parse_error_to_string error)
  | ParserReader.Need_more -> Result.Error "Expected reader parse error"
  | ParserReader.Frame _ -> Result.Error "Expected reader parse error, but frame parsed"

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
  let serialized = serialize_frame frame in
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
  let serialized = serialize_frame frame in
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
  let serialized = serialize_frame frame in
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
  let serialized = serialize_frame frame in
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
  let serialized = serialize_frame frame in
  let length = frame_payload_length serialized in
  if Int.equal length 0 && Int.equal (String.length serialized) 9 then
    Result.Ok ()
  else
    Result.Error ("Serialized settings ack should have empty payload, got length "
    ^ Int.to_string length)

let test_serialize_rejects_payload_mismatch = fun _ctx ->
  let frame = {
    Frame.length = 0;
    frame_type = Frame.Ping;
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
  match Serializer.serialize_frame frame with
  | Error (Serializer.PayloadMismatch { frame_type = Frame.Ping; _ }) -> Result.Ok ()
  | Error error -> Result.Error ("Wrong serializer error: " ^ Serializer.error_to_string error)
  | Ok _ -> Result.Error "serializer accepted mismatched PING payload"

let test_ping_rejects_invalid_payload_length = fun _ctx ->
  match Frame.ping "short" with
  | Error (Frame.InvalidPingPayloadLength { length = 5 }) -> Result.Ok ()
  | Error error -> Result.Error ("Wrong frame error: " ^ Frame.constructor_error_to_string error)
  | Ok _ -> Result.Error "PING accepted an invalid opaque data length"

let test_window_update_rejects_invalid_increment = fun _ctx ->
  match Frame.window_update ~stream_id:0 0 with
  | Error (Frame.InvalidWindowUpdateIncrement { increment = 0 }) -> Result.Ok ()
  | Error error -> Result.Error ("Wrong frame error: " ^ Frame.constructor_error_to_string error)
  | Ok _ -> Result.Error "WINDOW_UPDATE accepted an invalid increment"

let test_connection_window_update_invalid_increment_preserves_state = fun _ctx ->
  let conn = Connection.create ~role:Connection.Server () in
  let before = Connection.connection_window_size conn in
  match Connection.send_window_update_connection conn ~increment:0 with
  | Error (Connection.FrameConstructorError (Frame.InvalidWindowUpdateIncrement { increment = 0 })) ->
      let after = Connection.connection_window_size conn in
      if Int.equal before after then
        Result.Ok ()
      else
        Result.Error "invalid WINDOW_UPDATE changed the connection window"
  | Error error -> Result.Error ("Wrong connection error: " ^ Connection.error_to_string error)
  | Ok _ -> Result.Error "connection accepted invalid WINDOW_UPDATE increment"

let test_client_preface_settings_payload_length = fun _ctx ->
  let conn = Connection.create ~role:Connection.Client () in
  let preface = send_preface conn in
  let length = frame_payload_length_at preface ~offset:24 in
  if Int.equal length 30 then
    Result.Ok ()
  else
    Result.Error ("Client preface settings length should be 30, got " ^ Int.to_string length)

let test_server_preface_settings_payload_length = fun _ctx ->
  let conn = Connection.create ~role:Connection.Server () in
  let preface = send_preface conn in
  let length = frame_payload_length preface in
  if Int.equal length 30 then
    Result.Ok ()
  else
    Result.Error ("Server preface settings length should be 30, got " ^ Int.to_string length)

let test_process_data_buffers_split_frame_header = fun _ctx ->
  let conn = Connection.create ~role:Connection.Server () in
  let frame = Frame.settings [ Frame.HeaderTableSize 1_024; ] in
  let bytes = serialize_frame frame in
  let first = String.sub bytes ~offset:0 ~len:4 in
  let rest = String.sub bytes ~offset:4 ~len:(String.length bytes - 4) in
  match Connection.process_data conn (Std.IO.Bytes.from_string first) with
  | Error err -> Result.Error (Connection.error_to_string err)
  | Ok events when List.length events != 0 ->
      Result.Error "split frame header should not emit events before the frame is complete"
  | Ok _ -> (
      match Connection.process_data conn (Std.IO.Bytes.from_string rest) with
      | Ok [ Connection.SettingsReceived [ Frame.HeaderTableSize size ] ] when Int.equal size 1_024 ->
          Result.Ok ()
      | Ok _ -> Result.Error "split frame header did not emit the expected settings event"
      | Error err -> Result.Error (Connection.error_to_string err)
    )

let test_process_data_buffers_split_frame_payload = fun _ctx ->
  let conn = Connection.create ~role:Connection.Server () in
  let frame = Frame.settings [ Frame.HeaderTableSize 1_024; Frame.MaxFrameSize 16_384; ] in
  let bytes = serialize_frame frame in
  let first = String.sub bytes ~offset:0 ~len:12 in
  let rest = String.sub bytes ~offset:12 ~len:(String.length bytes - 12) in
  match Connection.process_data conn (Std.IO.Bytes.from_string first) with
  | Error err -> Result.Error (Connection.error_to_string err)
  | Ok events when List.length events != 0 ->
      Result.Error "split frame payload should not emit events before the frame is complete"
  | Ok _ -> (
      match Connection.process_data conn (Std.IO.Bytes.from_string rest) with
      | Ok [ Connection.SettingsReceived [ Frame.HeaderTableSize table_size; Frame.MaxFrameSize frame_size ] ] when Int.equal
        table_size
        1_024
      && Int.equal frame_size 16_384 -> Result.Ok ()
      | Ok _ -> Result.Error "split frame payload did not emit the expected settings event"
      | Error err -> Result.Error (Connection.error_to_string err)
    )

let test_reader_parser_parses_full_data_frame = fun _ctx ->
  let frame = Frame.data ~stream_id:1 "hello" in
  let bytes = serialize_frame frame in
  let parser = ParserReader.create () in
  let reader = Std.IO.Reader.from_string bytes in
  match ParserReader.parse parser reader with
  | ParserReader.Frame { Frame.frame_type = Frame.Data; stream_id = 1; payload = Frame.DataPayload { data = "hello"; pad_length = None }; _ } ->
      Result.Ok ()
  | ParserReader.Frame _ -> Result.Error "reader parser returned the wrong DATA frame"
  | ParserReader.Need_more -> Result.Error "reader parser unexpectedly needed more data"
  | ParserReader.Error error ->
      Result.Error ("reader parser failed: " ^ ParserReader.parse_error_to_string error)

let test_reader_parser_uses_canonical_payload_errors = fun _ctx ->
  let bytes = "\x00\x00\x07\x06\x00\x00\x00\x00\x00" ^ "1234567" in
  let parser = ParserReader.create () in
  let reader = Std.IO.Reader.from_string bytes in
  match ParserReader.parse parser reader with
  | ParserReader.Error (
    ParserReader.FrameParseFailed (
      Parser.InvalidPayloadLength { frame_type = Frame.Ping; expected = Parser.Exactly 8; actual = 7 }
    )
  ) -> Result.Ok ()
  | ParserReader.Error error ->
      Result.Error ("wrong reader parser error: " ^ ParserReader.parse_error_to_string error)
  | ParserReader.Need_more -> Result.Error "expected canonical payload error"
  | ParserReader.Frame _ -> Result.Error "invalid PING payload was accepted"

let test_reader_parser_uses_canonical_header_errors = fun _ctx ->
  let frame = Frame.data ~stream_id:1 "hello" in
  let bytes = serialize_frame frame in
  let parser = ParserReader.create ~config:{ ParserReader.max_frame_size = 1 } () in
  let reader = Std.IO.Reader.from_string bytes in
  match ParserReader.parse parser reader with
  | ParserReader.Error (
    ParserReader.FrameParseFailed (Parser.FrameSizeExceedsMaximum { size = 5; max_size = 1 })
  ) -> Result.Ok ()
  | ParserReader.Error error ->
      Result.Error ("wrong reader parser error: " ^ ParserReader.parse_error_to_string error)
  | ParserReader.Need_more -> Result.Error "expected canonical frame size error"
  | ParserReader.Frame _ -> Result.Error "oversized frame was accepted"

let test_reader_parser_rejects_data_stream_zero = fun _ctx ->
  expect_reader_parse_error
    "\x00\x00\x00\x00\x00\x00\x00\x00\x00"
    (Parser.InvalidStreamId {
      frame_type = Frame.Data;
      stream_id = 0;
      expected = Parser.MustBeNonZero;
    })

let test_reader_parser_rejects_settings_nonzero_stream = fun _ctx ->
  expect_reader_parse_error
    "\x00\x00\x00\x04\x00\x00\x00\x00\x01"
    (Parser.InvalidStreamId {
      frame_type = Frame.Settings;
      stream_id = 1;
      expected = Parser.MustBeZero;
    })

let test_reader_parser_rejects_invalid_enable_push = fun _ctx ->
  expect_reader_parse_error
    (settings_frame_with_payload "\x00\x02\x00\x00\x00\x02")
    (Parser.InvalidSettingValue { setting = Parser.EnablePush; value = 2 })

let test_reader_parser_rejects_zero_window_update_increment = fun _ctx ->
  expect_reader_parse_error
    "\x00\x00\x04\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00"
    Parser.WindowUpdateIncrementZero

let test_frame_types = fun _ctx ->
  let types = [ Frame.Data; Frame.Headers; Frame.Settings; Frame.Ping; Frame.Goaway; ] in
  if List.length types = 5 then
    Result.Ok ()
  else
    Result.Error "Frame types count mismatch"

let test_parse_settings_rejects_invalid_enable_push = fun _ctx ->
  let bytes = settings_frame_with_payload "\x00\x02\x00\x00\x00\x02" in
  match Parser.parse_frame bytes with
  | Parser.Error (Parser.InvalidSettingValue { setting = Parser.EnablePush; value = 2 }) ->
      Result.Ok ()
  | Parser.Error err -> Result.Error ("Wrong parse error: " ^ Parser.error_to_string err)
  | Parser.Need_more -> Result.Error "Expected invalid SETTINGS_ENABLE_PUSH to fail"
  | Parser.Done _ -> Result.Error "Invalid SETTINGS_ENABLE_PUSH was accepted"

let test_parse_settings_rejects_initial_window_overflow = fun _ctx ->
  let bytes = settings_frame_with_payload "\x00\x04\x80\x00\x00\x00" in
  match Parser.parse_frame bytes with
  | Parser.Error (
    Parser.InvalidSettingValue { setting = Parser.InitialWindowSize; value = 2_147_483_648 }
  ) -> Result.Ok ()
  | Parser.Error err -> Result.Error ("Wrong parse error: " ^ Parser.error_to_string err)
  | Parser.Need_more -> Result.Error "Expected oversized SETTINGS_INITIAL_WINDOW_SIZE to fail"
  | Parser.Done _ -> Result.Error "Oversized SETTINGS_INITIAL_WINDOW_SIZE was accepted"

let test_parse_settings_rejects_small_max_frame_size = fun _ctx ->
  let bytes = settings_frame_with_payload "\x00\x05\x00\x00\x3f\xff" in
  match Parser.parse_frame bytes with
  | Parser.Error (Parser.InvalidSettingValue { setting = Parser.MaxFrameSize; value = 16_383 }) ->
      Result.Ok ()
  | Parser.Error err -> Result.Error ("Wrong parse error: " ^ Parser.error_to_string err)
  | Parser.Need_more -> Result.Error "Expected small SETTINGS_MAX_FRAME_SIZE to fail"
  | Parser.Done _ -> Result.Error "Small SETTINGS_MAX_FRAME_SIZE was accepted"

let test_parse_settings_rejects_large_max_frame_size = fun _ctx ->
  let bytes = settings_frame_with_payload "\x00\x05\x01\x00\x00\x00" in
  match Parser.parse_frame bytes with
  | Parser.Error (Parser.InvalidSettingValue { setting = Parser.MaxFrameSize; value = 16_777_216 }) ->
      Result.Ok ()
  | Parser.Error err -> Result.Error ("Wrong parse error: " ^ Parser.error_to_string err)
  | Parser.Need_more -> Result.Error "Expected large SETTINGS_MAX_FRAME_SIZE to fail"
  | Parser.Done _ -> Result.Error "Large SETTINGS_MAX_FRAME_SIZE was accepted"

let test_parse_data_rejects_stream_zero = fun _ctx ->
  expect_parse_error
    "\x00\x00\x00\x00\x00\x00\x00\x00\x00"
    (Parser.InvalidStreamId {
      frame_type = Frame.Data;
      stream_id = 0;
      expected = Parser.MustBeNonZero;
    })

let test_parse_headers_rejects_stream_zero = fun _ctx ->
  expect_parse_error
    "\x00\x00\x00\x01\x00\x00\x00\x00\x00"
    (Parser.InvalidStreamId {
      frame_type = Frame.Headers;
      stream_id = 0;
      expected = Parser.MustBeNonZero;
    })

let test_parse_settings_rejects_nonzero_stream = fun _ctx ->
  expect_parse_error
    "\x00\x00\x00\x04\x00\x00\x00\x00\x01"
    (Parser.InvalidStreamId {
      frame_type = Frame.Settings;
      stream_id = 1;
      expected = Parser.MustBeZero;
    })

let test_parse_ping_rejects_nonzero_stream = fun _ctx ->
  expect_parse_error
    "\x00\x00\x08\x06\x00\x00\x00\x00\x01abcdefgh"
    (Parser.InvalidStreamId { frame_type = Frame.Ping; stream_id = 1; expected = Parser.MustBeZero })

let test_parse_goaway_rejects_nonzero_stream = fun _ctx ->
  expect_parse_error
    "\x00\x00\x08\x07\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00"
    (Parser.InvalidStreamId {
      frame_type = Frame.Goaway;
      stream_id = 1;
      expected = Parser.MustBeZero;
    })

let test_parse_window_update_allows_stream_zero = fun _ctx ->
  match Parser.parse_frame "\x00\x00\x04\x08\x00\x00\x00\x00\x00\x00\x00\x00\x01" with
  | Parser.Done { value = { Frame.frame_type = Frame.WindowUpdate; stream_id = 0; _ }; _ } ->
      Result.Ok ()
  | Parser.Done _ -> Result.Error "WINDOW_UPDATE stream 0 parsed as the wrong frame"
  | Parser.Need_more -> Result.Error "WINDOW_UPDATE stream 0 unexpectedly needed more data"
  | Parser.Error err ->
      Result.Error ("WINDOW_UPDATE stream 0 was rejected: " ^ Parser.error_to_string err)

let test_parse_unknown_frame_preserves_payload = fun _ctx ->
  match Parser.parse_frame "\x00\x00\x03\x0b\x00\x00\x00\x00\x00abc" with
  | Parser.Done { value = { Frame.frame_type = Frame.Unknown 0x0b; stream_id = 0; payload = Frame.UnknownPayload "abc"; _ }; remaining = "" } ->
      Result.Ok ()
  | Parser.Done _ -> Result.Error "Unknown frame parsed with the wrong payload"
  | Parser.Need_more -> Result.Error "Unknown frame unexpectedly needed more data"
  | Parser.Error err -> Result.Error ("Unknown frame was rejected: " ^ Parser.error_to_string err)

let test_process_data_ignores_unknown_frame = fun _ctx ->
  let conn = Connection.create ~role:Connection.Server () in
  match Connection.process_data
    conn
    (Std.IO.Bytes.from_string "\x00\x00\x03\x0b\x00\x00\x00\x00\x00abc") with
  | Ok [] -> Result.Ok ()
  | Ok _ -> Result.Error "Unknown frame emitted connection events"
  | Error err ->
      Result.Error ("Unknown frame failed connection processing: " ^ Connection.error_to_string err)

let test_process_data_rejects_unexpected_continuation = fun _ctx ->
  let conn = Connection.create ~role:Connection.Server () in
  let frame = Frame.continuation ~stream_id:1 ~end_headers:true "" in
  match Connection.process_data conn (Std.IO.Bytes.from_string (serialize_frame frame)) with
  | Error (Connection.UnexpectedContinuation { stream_id = 1 }) -> Result.Ok ()
  | Error err -> Result.Error ("Wrong connection error: " ^ Connection.error_to_string err)
  | Ok _ -> Result.Error "Unexpected CONTINUATION was accepted"

let test_process_data_requires_continuation_after_headers = fun _ctx ->
  let conn = Connection.create ~role:Connection.Server () in
  let headers = Frame.headers ~stream_id:1 ~end_headers:false "" in
  let data = Frame.data ~stream_id:1 "hello" in
  match Connection.process_data
    conn
    (Std.IO.Bytes.from_string (serialize_frame headers ^ serialize_frame data)) with
  | Error (Connection.ExpectedContinuation { stream_id = 1; frame_type = Frame.Data }) ->
      Result.Ok ()
  | Error err -> Result.Error ("Wrong connection error: " ^ Connection.error_to_string err)
  | Ok _ -> Result.Error "DATA frame was accepted before CONTINUATION completed the header block"

let test_process_data_rejects_continuation_stream_mismatch = fun _ctx ->
  let conn = Connection.create ~role:Connection.Server () in
  let headers = Frame.headers ~stream_id:1 ~end_headers:false "" in
  let continuation = Frame.continuation ~stream_id:3 ~end_headers:true "" in
  match Connection.process_data
    conn
    (Std.IO.Bytes.from_string (serialize_frame headers ^ serialize_frame continuation)) with
  | Error (Connection.ContinuationStreamMismatch { expected_stream_id = 1; actual_stream_id = 3 }) ->
      Result.Ok ()
  | Error err -> Result.Error ("Wrong connection error: " ^ Connection.error_to_string err)
  | Ok _ -> Result.Error "CONTINUATION on the wrong stream was accepted"

let test_process_data_accepts_split_header_block = fun _ctx ->
  let conn = Connection.create ~role:Connection.Server () in
  let header_block = encode_header_block [ { Hpack.name = ":method"; value = "GET" }; ] in
  let headers = Frame.headers ~stream_id:1 ~end_headers:false "" in
  let continuation = Frame.continuation ~stream_id:1 ~end_headers:true header_block in
  let data = Frame.data ~stream_id:1 "ok" in
  match Connection.process_data
    conn
    (Std.IO.Bytes.from_string
      (serialize_frame headers ^ serialize_frame continuation ^ serialize_frame data)) with
  | Ok [ Connection.HeadersReceived { stream_id = 1; headers = [ { Hpack.name = ":method"; value = "GET" } ]; end_stream = false }; Connection.DataReceived { stream_id = 1; data; end_stream = false } ] when Std.IO.Bytes.to_string
    data
  = "ok" -> Result.Ok ()
  | Ok _ -> Result.Error "Split header block did not emit the expected headers and data events"
  | Error err -> Result.Error ("Split header block failed: " ^ Connection.error_to_string err)

let test_process_data_rejects_data_before_headers = fun _ctx ->
  let conn = Connection.create ~role:Connection.Server () in
  let data = Frame.data ~stream_id:1 "hello" in
  match Connection.process_data conn (Std.IO.Bytes.from_string (serialize_frame data)) with
  | Error (Connection.DataBeforeHeaders { stream_id = 1 }) -> Result.Ok ()
  | Error err -> Result.Error ("Wrong connection error: " ^ Connection.error_to_string err)
  | Ok _ -> Result.Error "DATA before HEADERS was accepted"

let test_process_data_accepts_data_after_headers = fun _ctx ->
  let conn = Connection.create ~role:Connection.Server () in
  let header_block = encode_header_block [ { Hpack.name = ":method"; value = "GET" }; ] in
  let headers = Frame.headers ~stream_id:1 ~end_headers:true header_block in
  let data = Frame.data ~stream_id:1 "hello" in
  match Connection.process_data
    conn
    (Std.IO.Bytes.from_string (serialize_frame headers ^ serialize_frame data)) with
  | Ok [ Connection.HeadersReceived { stream_id = 1; headers = [ { Hpack.name = ":method"; value = "GET" } ]; end_stream = false }; Connection.DataReceived { stream_id = 1; data; end_stream = false } ] when Std.IO.Bytes.to_string
    data
  = "hello" -> Result.Ok ()
  | Ok _ -> Result.Error "DATA after HEADERS did not emit the expected events"
  | Error err -> Result.Error ("DATA after HEADERS failed: " ^ Connection.error_to_string err)

let tests =
  Test.[
    case "serialize_settings_frame" test_serialize_settings_frame;
    case "serialize_data_frame" test_serialize_data_frame;
    case "serialize_recomputes_payload_length" test_serialize_recomputes_payload_length;
    case "serialize_settings_payload_length" test_serialize_settings_payload_length;
    case "serialize_settings_ack_has_empty_payload" test_serialize_settings_ack_has_empty_payload;
    case "serialize_rejects_payload_mismatch" test_serialize_rejects_payload_mismatch;
    case "ping_rejects_invalid_payload_length" test_ping_rejects_invalid_payload_length;
    case "window_update_rejects_invalid_increment" test_window_update_rejects_invalid_increment;
    case
      "connection_window_update_invalid_increment_preserves_state"
      test_connection_window_update_invalid_increment_preserves_state;
    case "client_preface_settings_payload_length" test_client_preface_settings_payload_length;
    case "server_preface_settings_payload_length" test_server_preface_settings_payload_length;
    case "process_data_buffers_split_frame_header" test_process_data_buffers_split_frame_header;
    case "process_data_buffers_split_frame_payload" test_process_data_buffers_split_frame_payload;
    case "reader_parser_parses_full_data_frame" test_reader_parser_parses_full_data_frame;
    case
      "reader_parser_uses_canonical_payload_errors"
      test_reader_parser_uses_canonical_payload_errors;
    case
      "reader_parser_uses_canonical_header_errors"
      test_reader_parser_uses_canonical_header_errors;
    case "reader_parser_rejects_data_stream_zero" test_reader_parser_rejects_data_stream_zero;
    case
      "reader_parser_rejects_settings_nonzero_stream"
      test_reader_parser_rejects_settings_nonzero_stream;
    case "reader_parser_rejects_invalid_enable_push" test_reader_parser_rejects_invalid_enable_push;
    case
      "reader_parser_rejects_zero_window_update_increment"
      test_reader_parser_rejects_zero_window_update_increment;
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
    case
      "process_data_rejects_unexpected_continuation"
      test_process_data_rejects_unexpected_continuation;
    case
      "process_data_requires_continuation_after_headers"
      test_process_data_requires_continuation_after_headers;
    case
      "process_data_rejects_continuation_stream_mismatch"
      test_process_data_rejects_continuation_stream_mismatch;
    case "process_data_accepts_split_header_block" test_process_data_accepts_split_header_block;
    case "process_data_rejects_data_before_headers" test_process_data_rejects_data_before_headers;
    case "process_data_accepts_data_after_headers" test_process_data_accepts_data_after_headers;
  ]

let main ~args:_ = Test.Cli.main ~name:"http:http2_parser" ~tests ~args:Env.args ()

let () = Runtime.run ~main ~args:Env.args ()
