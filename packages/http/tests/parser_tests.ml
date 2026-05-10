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

let client_connection_preface = "PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n"

let peer_initial_settings = fun settings -> serialize_frame (Frame.settings settings)

let create_server_connection = fun ?config () ->
  let conn =
    match config with
    | Some config -> Connection.create ~role:Connection.Server ~config ()
    | None -> Connection.create ~role:Connection.Server ()
  in
  let _ = send_preface conn in
  match Connection.process_data
    conn
    (Std.IO.Bytes.from_string (client_connection_preface ^ peer_initial_settings [])) with
  | Ok [ Connection.SettingsReceived [] ] -> conn
  | Ok _ -> panic "server preface activation emitted unexpected events"
  | Error error -> panic ("server preface activation failed: " ^ Connection.error_to_string error)

let create_client_connection = fun ?config () ->
  let conn =
    match config with
    | Some config -> Connection.create ~role:Connection.Client ~config ()
    | None -> Connection.create ~role:Connection.Client ()
  in
  let _ = send_preface conn in
  match Connection.process_data conn (Std.IO.Bytes.from_string (peer_initial_settings [])) with
  | Ok [ Connection.SettingsReceived [] ] -> conn
  | Ok _ -> panic "client preface activation emitted unexpected events"
  | Error error -> panic ("client preface activation failed: " ^ Connection.error_to_string error)

let frame_payload_length_at = fun serialized ~offset ->
  let byte index = Char.code (String.get_unchecked serialized ~at:(offset + index)) in
  (byte 0 lsl 16) lor (byte 1 lsl 8) lor byte 2

let frame_payload_length = fun serialized -> frame_payload_length_at serialized ~offset:0

let frame_type_at = fun serialized ~offset ->
  Char.code
    (String.get_unchecked serialized ~at:(offset + 3))

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
    payload = Frame.SettingsPayload [];
  }
  in
  let serialized = serialize_frame frame in
  let length = frame_payload_length serialized in
  if Int.equal length 0 && Int.equal (String.length serialized) 9 then
    Result.Ok ()
  else
    Result.Error ("Serialized settings ack should have empty payload, got length "
    ^ Int.to_string length)

let test_serialize_settings_ack_rejects_payload = fun _ctx ->
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
  match Serializer.serialize_frame frame with
  | Error (Serializer.SettingsAckWithPayload { setting_count = 1 }) -> Result.Ok ()
  | Error error -> Result.Error ("Wrong serializer error: " ^ Serializer.error_to_string error)
  | Ok _ -> Result.Error "serializer accepted SETTINGS ACK with payload"

let test_serialize_rejects_invalid_initial_window_setting = fun _ctx ->
  let frame = Frame.settings [ Frame.InitialWindowSize 2_147_483_648; ] in
  match Serializer.serialize_frame frame with
  | Error (
    Serializer.InvalidSettingValue {
      setting = Serializer.InitialWindowSize;
      value = 2_147_483_648;
      expected = Serializer.InitialWindowSizeRange;
    }
  ) ->
      Result.Ok ()
  | Error error -> Result.Error ("Wrong serializer error: " ^ Serializer.error_to_string error)
  | Ok _ -> Result.Error "serializer accepted invalid SETTINGS_INITIAL_WINDOW_SIZE"

let test_serialize_rejects_invalid_max_frame_size_setting = fun _ctx ->
  let frame = Frame.settings [ Frame.MaxFrameSize 16_383; ] in
  match Serializer.serialize_frame frame with
  | Error (
    Serializer.InvalidSettingValue {
      setting = Serializer.MaxFrameSize;
      value = 16_383;
      expected = Serializer.MaxFrameSizeRange;
    }
  ) ->
      Result.Ok ()
  | Error error -> Result.Error ("Wrong serializer error: " ^ Serializer.error_to_string error)
  | Ok _ -> Result.Error "serializer accepted invalid SETTINGS_MAX_FRAME_SIZE"

let test_serialize_rejects_negative_uint32_setting = fun _ctx ->
  let frame = Frame.settings [ Frame.MaxHeaderListSize (-1); ] in
  match Serializer.serialize_frame frame with
  | Error (
    Serializer.InvalidSettingValue {
      setting = Serializer.MaxHeaderListSize;
      value = -1;
      expected = Serializer.Unsigned32;
    }
  ) ->
      Result.Ok ()
  | Error error -> Result.Error ("Wrong serializer error: " ^ Serializer.error_to_string error)
  | Ok _ -> Result.Error "serializer accepted negative SETTINGS_MAX_HEADER_LIST_SIZE"

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

let test_serialize_rejects_invalid_ping_payload_length = fun _ctx ->
  let frame = {
    Frame.length = 5;
    frame_type = Frame.Ping;
    flags = Frame.default_flags;
    stream_id = 0;
    payload = Frame.PingPayload "short";
  }
  in
  match Serializer.serialize_frame frame with
  | Error (Serializer.InvalidPingPayloadLength { length = 5 }) -> Result.Ok ()
  | Error error -> Result.Error ("Wrong serializer error: " ^ Serializer.error_to_string error)
  | Ok _ -> Result.Error "serializer accepted invalid PING payload length"

let test_serialize_rejects_invalid_window_update_increment = fun _ctx ->
  let frame = {
    Frame.length = 4;
    frame_type = Frame.WindowUpdate;
    flags = Frame.default_flags;
    stream_id = 0;
    payload = Frame.WindowUpdatePayload 0;
  }
  in
  match Serializer.serialize_frame frame with
  | Error (Serializer.InvalidWindowUpdateIncrement { increment = 0 }) -> Result.Ok ()
  | Error error -> Result.Error ("Wrong serializer error: " ^ Serializer.error_to_string error)
  | Ok _ -> Result.Error "serializer accepted invalid WINDOW_UPDATE increment"

let test_serialize_rejects_payload_length_overflow = fun _ctx ->
  let payload = String.make ~len:0x0100_0000 ~char:'x' in
  let frame = Frame.data ~stream_id:1 payload in
  match Serializer.serialize_frame frame with
  | Error (Serializer.PayloadLengthTooLarge { length = 0x0100_0000; max_length = 0x00ff_ffff }) ->
      Result.Ok ()
  | Error error -> Result.Error ("Wrong serializer error: " ^ Serializer.error_to_string error)
  | Ok _ -> Result.Error "serializer accepted payload larger than the HTTP/2 frame length field"

let test_parse_frame_respects_configured_max_frame_size = fun _ctx ->
  let payload = String.make ~len:20_000 ~char:'x' in
  let frame = Frame.data ~stream_id:1 payload in
  let bytes = serialize_frame frame in
  let config = { Parser.max_frame_size = 20_000 } in
  match Parser.parse_frame ~config bytes with
  | Parser.Done { value = { Frame.payload = Frame.DataPayload { data; _ }; _ }; remaining = "" } when String.length
    data
  = 20_000 -> Result.Ok ()
  | Parser.Done _ -> Result.Error "configured parser produced the wrong frame"
  | Parser.Need_more -> Result.Error "configured parser treated complete frame as incomplete"
  | Parser.Error error ->
      Result.Error ("configured parser rejected frame: " ^ Parser.error_to_string error)

let test_parse_frame_rejects_over_configured_max_frame_size = fun _ctx ->
  let payload = String.make ~len:20_001 ~char:'x' in
  let frame = Frame.data ~stream_id:1 payload in
  let bytes = serialize_frame frame in
  let config = { Parser.max_frame_size = 20_000 } in
  match Parser.parse_frame ~config bytes with
  | Parser.Error (Parser.FrameSizeExceedsMaximum { size = 20_001; max_size = 20_000 }) ->
      Result.Ok ()
  | Parser.Error error -> Result.Error ("Wrong parser error: " ^ Parser.error_to_string error)
  | Parser.Need_more -> Result.Error "oversized frame was treated as incomplete"
  | Parser.Done _ -> Result.Error "oversized frame parsed successfully"

let test_serialize_rejects_invalid_unknown_frame_type_code = fun _ctx ->
  let frame = {
    Frame.length = 0;
    frame_type = Frame.Unknown 300;
    flags = Frame.default_flags;
    stream_id = 0;
    payload = Frame.UnknownPayload "";
  }
  in
  match Serializer.serialize_frame frame with
  | Error (Serializer.InvalidUnknownFrameTypeCode { code = 300 }) -> Result.Ok ()
  | Error error -> Result.Error ("Wrong serializer error: " ^ Serializer.error_to_string error)
  | Ok _ -> Result.Error "serializer accepted an unknown frame code wider than one byte"

let test_serialize_rejects_zero_stream_data_frame = fun _ctx ->
  let frame = Frame.data ~stream_id:0 "hello" in
  match Serializer.serialize_frame frame with
  | Error (
    Serializer.InvalidStreamId {
      frame_type = Frame.Data;
      stream_id = 0;
      expected = Serializer.MustBeNonZero;
    }
  ) ->
      Result.Ok ()
  | Error error -> Result.Error ("Wrong serializer error: " ^ Serializer.error_to_string error)
  | Ok _ -> Result.Error "serializer accepted DATA frame on stream 0"

let test_serialize_rejects_nonzero_stream_settings_frame = fun _ctx ->
  let frame = {
    Frame.length = 0;
    frame_type = Frame.Settings;
    flags = Frame.default_flags;
    stream_id = 1;
    payload = Frame.SettingsPayload [];
  }
  in
  match Serializer.serialize_frame frame with
  | Error (
    Serializer.InvalidStreamId {
      frame_type = Frame.Settings;
      stream_id = 1;
      expected = Serializer.MustBeZero;
    }
  ) ->
      Result.Ok ()
  | Error error -> Result.Error ("Wrong serializer error: " ^ Serializer.error_to_string error)
  | Ok _ -> Result.Error "serializer accepted SETTINGS frame on a non-zero stream"

let test_serialize_rejects_invalid_padding_length = fun _ctx ->
  let frame = Frame.data ~stream_id:1 ~pad_length:(-1) "hello" in
  match Serializer.serialize_frame frame with
  | Error (Serializer.InvalidPaddingLength { frame_type = Frame.Data; pad_length = -1 }) ->
      Result.Ok ()
  | Error error -> Result.Error ("Wrong serializer error: " ^ Serializer.error_to_string error)
  | Ok _ -> Result.Error "serializer accepted negative DATA padding"

let test_serialize_rejects_invalid_priority_weight = fun _ctx ->
  let frame = Frame.priority ~stream_id:1 ~stream_dependency:0 ~exclusive:false ~weight:0 in
  match Serializer.serialize_frame frame with
  | Error (Serializer.InvalidPriorityWeight { weight = 0 }) -> Result.Ok ()
  | Error error -> Result.Error ("Wrong serializer error: " ^ Serializer.error_to_string error)
  | Ok _ -> Result.Error "serializer accepted invalid priority weight"

let test_serialize_rejects_invalid_stream_dependency = fun _ctx ->
  let frame = Frame.priority ~stream_id:1 ~stream_dependency:(-1) ~exclusive:false ~weight:1 in
  match Serializer.serialize_frame frame with
  | Error (Serializer.InvalidStreamDependency { stream_dependency = -1 }) -> Result.Ok ()
  | Error error -> Result.Error ("Wrong serializer error: " ^ Serializer.error_to_string error)
  | Ok _ -> Result.Error "serializer accepted invalid stream dependency"

let test_serialize_rejects_self_priority_dependency = fun _ctx ->
  let frame = Frame.priority ~stream_id:3 ~stream_dependency:3 ~exclusive:false ~weight:1 in
  match Serializer.serialize_frame frame with
  | Error (Serializer.InvalidPriorityDependency { stream_id = 3; stream_dependency = 3 }) ->
      Result.Ok ()
  | Error error -> Result.Error ("Wrong serializer error: " ^ Serializer.error_to_string error)
  | Ok _ -> Result.Error "serializer accepted a self-dependent PRIORITY frame"

let test_serialize_rejects_self_headers_priority_dependency = fun _ctx ->
  let frame = Frame.headers ~stream_id:3 ~priority:(3, false, 1) "" in
  match Serializer.serialize_frame frame with
  | Error (Serializer.InvalidPriorityDependency { stream_id = 3; stream_dependency = 3 }) ->
      Result.Ok ()
  | Error error -> Result.Error ("Wrong serializer error: " ^ Serializer.error_to_string error)
  | Ok _ -> Result.Error "serializer accepted a self-dependent HEADERS priority"

let test_serialize_rejects_incomplete_headers_priority = fun _ctx ->
  let frame = {
    Frame.length = 0;
    frame_type = Frame.Headers;
    flags = { Frame.default_flags with priority = true };
    stream_id = 1;
    payload =
      Frame.HeadersPayload {
        pad_length = None;
        stream_dependency = Some 0;
        weight = None;
        exclusive = false;
        header_block_fragment = "";
      };
  }
  in
  match Serializer.serialize_frame frame with
  | Error (Serializer.MissingPriorityFields { frame_type = Frame.Headers }) -> Result.Ok ()
  | Error error -> Result.Error ("Wrong serializer error: " ^ Serializer.error_to_string error)
  | Ok _ -> Result.Error "serializer accepted incomplete HEADERS priority fields"

let test_serialize_rejects_negative_stream_id = fun _ctx ->
  let frame = Frame.data ~stream_id:(-1) "hello" in
  match Serializer.serialize_frame frame with
  | Error (Serializer.InvalidStreamIdRange { stream_id = -1 }) -> Result.Ok ()
  | Error error -> Result.Error ("Wrong serializer error: " ^ Serializer.error_to_string error)
  | Ok _ -> Result.Error "serializer accepted a negative stream ID"

let test_serialize_rejects_invalid_promised_stream_id = fun _ctx ->
  let frame = Frame.push_promise ~stream_id:1 ~promised_stream_id:0 "" in
  match Serializer.serialize_frame frame with
  | Error (Serializer.InvalidPromisedStreamId { promised_stream_id = 0 }) -> Result.Ok ()
  | Error error -> Result.Error ("Wrong serializer error: " ^ Serializer.error_to_string error)
  | Ok _ -> Result.Error "serializer accepted invalid promised stream ID"

let test_serialize_rejects_invalid_last_stream_id = fun _ctx ->
  let frame = Frame.goaway ~last_stream_id:(-1) ~error_code:Frame.NoError () in
  match Serializer.serialize_frame frame with
  | Error (Serializer.InvalidLastStreamId { last_stream_id = -1 }) -> Result.Ok ()
  | Error error -> Result.Error ("Wrong serializer error: " ^ Serializer.error_to_string error)
  | Ok _ -> Result.Error "serializer accepted invalid GOAWAY last stream ID"

let test_serialize_preserves_unknown_error_code = fun _ctx ->
  let frame = Frame.rst_stream ~stream_id:1 (Frame.UnknownErrorCode 0xfeed_beef) in
  let serialized = serialize_frame frame in
  let error_code = String.sub serialized ~offset:9 ~len:4 in
  if error_code = "\xfe\xed\xbe\xef" then
    Result.Ok ()
  else
    Result.Error "serializer did not preserve unknown HTTP/2 error code"

let test_serialize_rejects_invalid_unknown_error_code = fun _ctx ->
  let frame = Frame.rst_stream ~stream_id:1 (Frame.UnknownErrorCode (-1)) in
  match Serializer.serialize_frame frame with
  | Error (Serializer.InvalidErrorCode { code = -1 }) -> Result.Ok ()
  | Error error -> Result.Error ("Wrong serializer error: " ^ Serializer.error_to_string error)
  | Ok _ -> Result.Error "serializer accepted invalid unknown HTTP/2 error code"

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
  let conn = create_server_connection () in
  let before = Connection.connection_window_size conn in
  match Connection.send_window_update_connection conn ~increment:0 with
  | Error (
    Connection.FrameConstructorError (
      Frame.InvalidWindowUpdateIncrement { increment = 0 }
    )
  ) ->
      let after = Connection.connection_window_size conn in
      if Int.equal before after then
        Result.Ok ()
      else
        Result.Error "invalid WINDOW_UPDATE changed the connection window"
  | Error error -> Result.Error ("Wrong connection error: " ^ Connection.error_to_string error)
  | Ok _ -> Result.Error "connection accepted invalid WINDOW_UPDATE increment"

let test_connection_window_update_increases_receive_window_only = fun _ctx ->
  let conn = create_server_connection () in
  let send_before = Connection.connection_window_size conn in
  let receive_before = Connection.receive_connection_window_size conn in
  match Connection.send_window_update_connection conn ~increment:10 with
  | Error error -> Result.Error ("WINDOW_UPDATE failed: " ^ Connection.error_to_string error)
  | Ok _ ->
      let send_after = Connection.connection_window_size conn in
      let receive_after = Connection.receive_connection_window_size conn in
      if not (Int.equal send_after send_before) then
        Result.Error "sending WINDOW_UPDATE changed the outbound send window"
      else if not (Int.equal receive_after (receive_before + 10)) then
        Result.Error "sending WINDOW_UPDATE did not increase the receive window"
      else
        Result.Ok ()

let test_send_data_splits_by_remote_max_frame_size = fun _ctx ->
  let conn = create_client_connection () in
  match Connection.create_stream conn with
  | Error error -> Result.Error ("Creating stream failed: " ^ Connection.error_to_string error)
  | Ok stream_id ->
      match Connection.send_headers
        conn
        ~stream_id
        ~headers:[ { Hpack.name = ":method"; value = "GET" }; ]
        ~end_stream:false with
      | Error error -> Result.Error ("Sending HEADERS failed: " ^ Connection.error_to_string error)
      | Ok _ ->
          let payload = Std.IO.Bytes.from_string (String.make ~len:20_000 ~char:'x') in
          match Connection.send_data conn ~stream_id ~data:payload ~end_stream:true with
          | Error error -> Result.Error ("Sending DATA failed: " ^ Connection.error_to_string error)
          | Ok bytes ->
              let first_len = frame_payload_length bytes in
              let second_offset = 9 + first_len in
              let second_len = frame_payload_length_at bytes ~offset:second_offset in
              if not (Int.equal first_len 16_384) then
                Result.Error ("First DATA frame length was " ^ Int.to_string first_len)
              else if not (Int.equal second_len (20_000 - 16_384)) then
                Result.Error ("Second DATA frame length was " ^ Int.to_string second_len)
              else if String.length bytes != (9 + first_len + 9 + second_len) then
                Result.Error "DATA split produced unexpected trailing bytes"
              else
                Result.Ok ()

let test_send_headers_splits_continuations_by_remote_max_frame_size = fun _ctx ->
  let conn = create_client_connection () in
  match Connection.create_stream conn with
  | Error error -> Result.Error ("Creating stream failed: " ^ Connection.error_to_string error)
  | Ok stream_id ->
      let large_value = String.make ~len:20_000 ~char:'a' in
      match Connection.send_headers
        conn
        ~stream_id
        ~headers:[ { Hpack.name = "x-large-header"; value = large_value }; ]
        ~end_stream:false with
      | Error error -> Result.Error ("Sending HEADERS failed: " ^ Connection.error_to_string error)
      | Ok bytes ->
          let first_len = frame_payload_length bytes in
          let second_offset = 9 + first_len in
          if not (Int.equal (frame_type_at bytes ~offset:0) 0x1) then
            Result.Error "First frame was not HEADERS"
          else if not (Int.equal first_len 16_384) then
            Result.Error ("First HEADERS frame length was " ^ Int.to_string first_len)
          else if not (Int.equal (frame_type_at bytes ~offset:second_offset) 0x9) then
            Result.Error "Second frame was not CONTINUATION"
          else
            Result.Ok ()

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

let test_server_preface_disables_push_by_default = fun _ctx ->
  let conn = Connection.create ~role:Connection.Server () in
  let preface = send_preface conn in
  match Parser.parse_frame preface with
  | Parser.Done { value = { Frame.payload = Frame.SettingsPayload settings; _ }; remaining = "" } ->
      (match settings with
      | [
          Frame.HeaderTableSize _;
          Frame.EnablePush false;
          Frame.MaxConcurrentStreams _;
          Frame.InitialWindowSize _;
          Frame.MaxFrameSize _;
        ] -> Result.Ok ()
      | _ -> Result.Error "server preface did not advertise EnablePush false")
  | Parser.Done _ -> Result.Error "server preface left trailing bytes or wrong payload"
  | Parser.Need_more -> Result.Error "server preface was incomplete"
  | Parser.Error error ->
      Result.Error ("server preface did not parse: " ^ Parser.error_to_string error)

let test_server_accepts_split_client_preface = fun _ctx ->
  let conn = Connection.create ~role:Connection.Server () in
  let _ = send_preface conn in
  let bytes = client_connection_preface ^ peer_initial_settings [] in
  let first = String.sub bytes ~offset:0 ~len:12 in
  let rest = String.sub bytes ~offset:12 ~len:(String.length bytes - 12) in
  match Connection.process_data conn (Std.IO.Bytes.from_string first) with
  | Error error -> Result.Error ("split preface failed early: " ^ Connection.error_to_string error)
  | Ok events when List.length events != 0 -> Result.Error "partial client preface emitted events"
  | Ok _ ->
      match Connection.process_data conn (Std.IO.Bytes.from_string rest) with
      | Ok [ Connection.SettingsReceived [] ] -> Result.Ok ()
      | Ok _ -> Result.Error "split client preface did not emit initial SETTINGS"
      | Error error ->
          Result.Error ("split client preface failed: " ^ Connection.error_to_string error)

let test_server_rejects_malformed_client_preface = fun _ctx ->
  let conn = Connection.create ~role:Connection.Server () in
  let _ = send_preface conn in
  let malformed =
    "X"
    ^ String.sub
      client_connection_preface
      ~offset:1
      ~len:(String.length client_connection_preface - 1)
  in
  match Connection.process_data conn (Std.IO.Bytes.from_string malformed) with
  | Error (Connection.InvalidClientPrefaceByte { offset = 0; expected = 80; actual = 88 }) ->
      Result.Ok ()
  | Error error -> Result.Error ("Wrong preface error: " ^ Connection.error_to_string error)
  | Ok _ -> Result.Error "Malformed client preface was accepted"

let test_client_rejects_non_settings_initial_frame = fun _ctx ->
  let conn = Connection.create ~role:Connection.Client () in
  let _ = send_preface conn in
  let ping =
    match Frame.ping "12345678" with
    | Ok frame -> frame
    | Error error -> panic ("PING construction failed: " ^ Frame.constructor_error_to_string error)
  in
  match Connection.process_data conn (Std.IO.Bytes.from_string (serialize_frame ping)) with
  | Error (Connection.ExpectedInitialSettings { frame_type = Frame.Ping }) -> Result.Ok ()
  | Error error -> Result.Error ("Wrong initial frame error: " ^ Connection.error_to_string error)
  | Ok _ -> Result.Error "Client accepted a non-SETTINGS initial frame"

let test_process_data_buffers_split_frame_header = fun _ctx ->
  let conn = create_server_connection () in
  let frame = Frame.settings [ Frame.HeaderTableSize 1_024; ] in
  let bytes = serialize_frame frame in
  let first = String.sub bytes ~offset:0 ~len:4 in
  let rest = String.sub bytes ~offset:4 ~len:(String.length bytes - 4) in
  match Connection.process_data conn (Std.IO.Bytes.from_string first) with
  | Error err -> Result.Error (Connection.error_to_string err)
  | Ok events when List.length events != 0 ->
      Result.Error "split frame header should not emit events before the frame is complete"
  | Ok _ ->
      match Connection.process_data conn (Std.IO.Bytes.from_string rest) with
      | Ok [
          Connection.SettingsReceived [ Frame.HeaderTableSize size ];
        ] when Int.equal size 1_024 -> Result.Ok ()
      | Ok _ -> Result.Error "split frame header did not emit the expected settings event"
      | Error err -> Result.Error (Connection.error_to_string err)

let test_process_data_buffers_split_frame_payload = fun _ctx ->
  let conn = create_server_connection () in
  let frame = Frame.settings [ Frame.HeaderTableSize 1_024; Frame.MaxFrameSize 16_384; ] in
  let bytes = serialize_frame frame in
  let first = String.sub bytes ~offset:0 ~len:12 in
  let rest = String.sub bytes ~offset:12 ~len:(String.length bytes - 12) in
  match Connection.process_data conn (Std.IO.Bytes.from_string first) with
  | Error err -> Result.Error (Connection.error_to_string err)
  | Ok events when List.length events != 0 ->
      Result.Error "split frame payload should not emit events before the frame is complete"
  | Ok _ ->
      match Connection.process_data conn (Std.IO.Bytes.from_string rest) with
      | Ok [
          Connection.SettingsReceived [
              Frame.HeaderTableSize table_size;
              Frame.MaxFrameSize frame_size;
            ];
        ] when Int.equal table_size 1_024 && Int.equal frame_size 16_384 -> Result.Ok ()
      | Ok _ -> Result.Error "split frame payload did not emit the expected settings event"
      | Error err -> Result.Error (Connection.error_to_string err)

let test_process_data_buffers_frame_one_byte_at_a_time = fun _ctx ->
  let conn = create_server_connection () in
  let frame = Frame.settings [ Frame.HeaderTableSize 2_048; ] in
  let bytes = serialize_frame frame in
  let rec loop index =
    if index >= String.length bytes then
      Result.Error "one-byte frame delivery finished without emitting settings"
    else
      let byte = String.sub bytes ~offset:index ~len:1 in
      match Connection.process_data conn (Std.IO.Bytes.from_string byte) with
      | Error err -> Result.Error (Connection.error_to_string err)
      | Ok [] -> loop (index + 1)
      | Ok [
          Connection.SettingsReceived [ Frame.HeaderTableSize size ];
        ] when Int.equal size 2_048 ->
          if index = String.length bytes - 1 then
            Result.Ok ()
          else
            Result.Error "settings emitted before the full frame arrived"
      | Ok _ -> Result.Error "one-byte delivery emitted unexpected events"
  in
  loop 0

let test_process_data_buffers_split_continuation_payload = fun _ctx ->
  let conn = create_server_connection () in
  let header_block =
    encode_header_block [ { Hpack.name = "x-split-continuation"; value = "payload" } ]
  in
  let headers = Frame.headers ~stream_id:1 ~end_headers:false "" in
  let continuation =
    serialize_frame (Frame.continuation ~stream_id:1 ~end_headers:true header_block)
  in
  let first = String.sub continuation ~offset:0 ~len:10 in
  let rest = String.sub continuation ~offset:10 ~len:(String.length continuation - 10) in
  match Connection.process_data conn (Std.IO.Bytes.from_string (serialize_frame headers)) with
  | Error err -> Result.Error ("HEADERS failed: " ^ Connection.error_to_string err)
  | Ok events when List.length events != 0 ->
      Result.Error "incomplete header block should not emit events"
  | Ok _ ->
      match Connection.process_data conn (Std.IO.Bytes.from_string first) with
      | Error err -> Result.Error ("partial CONTINUATION failed: " ^ Connection.error_to_string err)
      | Ok events when List.length events != 0 ->
          Result.Error "partial CONTINUATION should not emit events"
      | Ok _ ->
          match Connection.process_data conn (Std.IO.Bytes.from_string rest) with
          | Ok [
              Connection.HeadersReceived {
                stream_id = 1;
                headers = [ { Hpack.name = "x-split-continuation"; value = "payload" } ];
                end_stream = false;
              };
            ] -> Result.Ok ()
          | Ok _ -> Result.Error "split CONTINUATION did not emit expected headers"
          | Error err ->
              Result.Error ("split CONTINUATION failed: " ^ Connection.error_to_string err)

let test_process_data_clears_pending_input_after_parse_error = fun _ctx ->
  let conn = create_server_connection () in
  let invalid_ping = "\x00\x00\x07\x06\x00\x00\x00\x00\x00" ^ "1234567" in
  let valid_settings = serialize_frame (Frame.settings [ Frame.HeaderTableSize 1_024; ]) in
  let first = String.sub invalid_ping ~offset:0 ~len:4 in
  let rest = String.sub invalid_ping ~offset:4 ~len:(String.length invalid_ping - 4) in
  match Connection.process_data conn (Std.IO.Bytes.from_string first) with
  | Error err ->
      Result.Error ("partial invalid frame failed early: " ^ Connection.error_to_string err)
  | Ok events when List.length events != 0 ->
      Result.Error "partial invalid frame should not emit events"
  | Ok _ ->
      (match Connection.process_data conn (Std.IO.Bytes.from_string rest) with
      | Error (
        Connection.ParserError (
          Parser.InvalidPayloadLength {
            frame_type = Frame.Ping;
            expected = Parser.Exactly 8;
            actual = 7;
          }
	        )
	      ) ->
	          (match Connection.process_data conn (Std.IO.Bytes.from_string valid_settings) with
	          | Ok [
	            Connection.SettingsReceived [ Frame.HeaderTableSize size ];
	          ] when Int.equal size 1_024 -> Result.Ok ()
	          | Ok _ -> Result.Error "valid frame after parse error emitted unexpected events"
	          | Error err ->
	              Result.Error ("pending input was not cleared after parse error: "
	              ^ Connection.error_to_string err))
	      | Error err -> Result.Error ("wrong parse error: " ^ Connection.error_to_string err)
      | Ok _ -> Result.Error "invalid frame was accepted")

let test_reader_parser_parses_full_data_frame = fun _ctx ->
  let frame = Frame.data ~stream_id:1 "hello" in
  let bytes = serialize_frame frame in
  let parser = ParserReader.create () in
  let reader = Std.IO.Reader.from_string bytes in
  match ParserReader.parse parser reader with
  | ParserReader.Frame {
      Frame.frame_type = Frame.Data;
      stream_id = 1;
      payload = Frame.DataPayload { data = "hello"; pad_length = None };
      _;
    } ->
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
  ) ->
      Result.Ok ()
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
    ParserReader.FrameParseFailed (
      Parser.FrameSizeExceedsMaximum { size = 5; max_size = 1 }
    )
  ) ->
      Result.Ok ()
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
    (Parser.InvalidSettingValue {
      setting = Parser.EnablePush;
      value = 2;
      expected = Parser.ZeroOrOne;
    })

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
  | Parser.Error (
    Parser.InvalidSettingValue { setting = Parser.EnablePush; value = 2; expected = Parser.ZeroOrOne }
  ) ->
      Result.Ok ()
  | Parser.Error err -> Result.Error ("Wrong parse error: " ^ Parser.error_to_string err)
  | Parser.Need_more -> Result.Error "Expected invalid SETTINGS_ENABLE_PUSH to fail"
  | Parser.Done _ -> Result.Error "Invalid SETTINGS_ENABLE_PUSH was accepted"

let test_parse_settings_rejects_initial_window_overflow = fun _ctx ->
  let bytes = settings_frame_with_payload "\x00\x04\x80\x00\x00\x00" in
  match Parser.parse_frame bytes with
  | Parser.Error (
    Parser.InvalidSettingValue {
      setting = Parser.InitialWindowSize;
      value = 2_147_483_648;
      expected = Parser.InitialWindowSizeRange;
    }
  ) ->
      Result.Ok ()
  | Parser.Error err -> Result.Error ("Wrong parse error: " ^ Parser.error_to_string err)
  | Parser.Need_more -> Result.Error "Expected oversized SETTINGS_INITIAL_WINDOW_SIZE to fail"
  | Parser.Done _ -> Result.Error "Oversized SETTINGS_INITIAL_WINDOW_SIZE was accepted"

let test_parse_settings_rejects_small_max_frame_size = fun _ctx ->
  let bytes = settings_frame_with_payload "\x00\x05\x00\x00\x3f\xff" in
  match Parser.parse_frame bytes with
  | Parser.Error (
    Parser.InvalidSettingValue {
      setting = Parser.MaxFrameSize;
      value = 16_383;
      expected = Parser.MaxFrameSizeRange;
    }
  ) ->
      Result.Ok ()
  | Parser.Error err -> Result.Error ("Wrong parse error: " ^ Parser.error_to_string err)
  | Parser.Need_more -> Result.Error "Expected small SETTINGS_MAX_FRAME_SIZE to fail"
  | Parser.Done _ -> Result.Error "Small SETTINGS_MAX_FRAME_SIZE was accepted"

let test_parse_settings_rejects_large_max_frame_size = fun _ctx ->
  let bytes = settings_frame_with_payload "\x00\x05\x01\x00\x00\x00" in
  match Parser.parse_frame bytes with
  | Parser.Error (
    Parser.InvalidSettingValue {
      setting = Parser.MaxFrameSize;
      value = 16_777_216;
      expected = Parser.MaxFrameSizeRange;
    }
  ) ->
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

let test_parse_data_rejects_padded_zero_length = fun _ctx ->
  expect_parse_error
    "\x00\x00\x00\x00\x08\x00\x00\x00\x01"
    (Parser.InvalidPayloadLength {
      frame_type = Frame.Data;
      expected = Parser.AtLeast 1;
      actual = 0;
    })

let test_parse_headers_rejects_padded_zero_length = fun _ctx ->
  expect_parse_error
    "\x00\x00\x00\x01\x08\x00\x00\x00\x01"
    (Parser.InvalidPayloadLength {
      frame_type = Frame.Headers;
      expected = Parser.AtLeast 1;
      actual = 0;
    })

let test_parse_headers_rejects_short_priority_payload = fun _ctx ->
  expect_parse_error
    "\x00\x00\x04\x01\x20\x00\x00\x00\x01abcd"
    (Parser.InvalidPayloadLength {
      frame_type = Frame.Headers;
      expected = Parser.AtLeast 5;
      actual = 4;
    })

let test_parse_push_promise_rejects_short_promised_stream = fun _ctx ->
  expect_parse_error
    "\x00\x00\x03\x05\x00\x00\x00\x00\x01abc"
    (Parser.InvalidPayloadLength {
      frame_type = Frame.PushPromise;
      expected = Parser.AtLeast 4;
      actual = 3;
    })

let test_parse_priority_rejects_self_dependency = fun _ctx ->
  expect_parse_error
    "\x00\x00\x05\x02\x00\x00\x00\x00\x03\x00\x00\x00\x03\x00"
    (Parser.InvalidPriorityDependency { stream_id = 3; stream_dependency = 3 })

let test_parse_headers_rejects_self_priority_dependency = fun _ctx ->
  expect_parse_error
    "\x00\x00\x05\x01\x20\x00\x00\x00\x03\x00\x00\x00\x03\x00"
    (Parser.InvalidPriorityDependency { stream_id = 3; stream_dependency = 3 })

let test_parse_rst_stream_preserves_unknown_error_code = fun _ctx ->
  match Parser.parse_frame "\x00\x00\x04\x03\x00\x00\x00\x00\x01\xfe\xed\xbe\xef" with
  | Parser.Done {
      value = {
        Frame.payload = Frame.RstStreamPayload (Frame.UnknownErrorCode code);
        _;
      };
      remaining = "";
    } when Int.equal code 0xfeed_beef ->
      Result.Ok ()
  | Parser.Done _ -> Result.Error "RST_STREAM unknown error code was not preserved"
  | Parser.Need_more -> Result.Error "RST_STREAM unexpectedly needed more data"
  | Parser.Error err -> Result.Error ("RST_STREAM parse failed: " ^ Parser.error_to_string err)

let test_parse_goaway_preserves_unknown_error_code = fun _ctx ->
  match Parser.parse_frame "\x00\x00\x08\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\xfe\xed\xbe\xef" with
  | Parser.Done {
      value = {
        Frame.payload = Frame.GoawayPayload { error_code = Frame.UnknownErrorCode code; _ };
        _;
      };
      remaining = "";
    } when Int.equal code 0xfeed_beef ->
      Result.Ok ()
  | Parser.Done _ -> Result.Error "GOAWAY unknown error code was not preserved"
  | Parser.Need_more -> Result.Error "GOAWAY unexpectedly needed more data"
  | Parser.Error err -> Result.Error ("GOAWAY parse failed: " ^ Parser.error_to_string err)

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
  | Parser.Done {
      value = {
        Frame.frame_type = Frame.Unknown 0x0b;
        stream_id = 0;
        payload = Frame.UnknownPayload "abc";
        _;
      };
      remaining = "";
    } ->
      Result.Ok ()
  | Parser.Done _ -> Result.Error "Unknown frame parsed with the wrong payload"
  | Parser.Need_more -> Result.Error "Unknown frame unexpectedly needed more data"
  | Parser.Error err -> Result.Error ("Unknown frame was rejected: " ^ Parser.error_to_string err)

let test_process_data_ignores_unknown_frame = fun _ctx ->
  let conn = create_server_connection () in
  match Connection.process_data
    conn
    (Std.IO.Bytes.from_string "\x00\x00\x03\x0b\x00\x00\x00\x00\x00abc") with
  | Ok [] -> Result.Ok ()
  | Ok _ -> Result.Error "Unknown frame emitted connection events"
  | Error err ->
      Result.Error ("Unknown frame failed connection processing: " ^ Connection.error_to_string err)

let test_process_data_rejects_push_promise = fun _ctx ->
  let conn = create_client_connection () in
  let frame = Frame.push_promise ~stream_id:2 ~promised_stream_id:4 "abc" in
  match Connection.process_data conn (Std.IO.Bytes.from_string (serialize_frame frame)) with
  | Error (
    Connection.UnsupportedFrameReceived {
      frame_type = Frame.PushPromise;
      payload = Frame.PushPromisePayload { promised_stream_id = 4; header_block_fragment = "abc"; _ };
    }
  ) ->
      Result.Ok ()
  | Error err -> Result.Error ("Wrong connection error: " ^ Connection.error_to_string err)
  | Ok _ -> Result.Error "PUSH_PROMISE was silently accepted"

let test_process_data_rejects_unexpected_continuation = fun _ctx ->
  let conn = create_server_connection () in
  let frame = Frame.continuation ~stream_id:1 ~end_headers:true "" in
  match Connection.process_data conn (Std.IO.Bytes.from_string (serialize_frame frame)) with
  | Error (Connection.UnexpectedContinuation { stream_id = 1 }) -> Result.Ok ()
  | Error err -> Result.Error ("Wrong connection error: " ^ Connection.error_to_string err)
  | Ok _ -> Result.Error "Unexpected CONTINUATION was accepted"

let test_process_data_requires_continuation_after_headers = fun _ctx ->
  let conn = create_server_connection () in
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
  let conn = create_server_connection () in
  let headers = Frame.headers ~stream_id:1 ~end_headers:false "" in
  let continuation = Frame.continuation ~stream_id:3 ~end_headers:true "" in
  match Connection.process_data
    conn
    (Std.IO.Bytes.from_string (serialize_frame headers ^ serialize_frame continuation)) with
  | Error (
    Connection.ContinuationStreamMismatch { expected_stream_id = 1; actual_stream_id = 3 }
  ) ->
      Result.Ok ()
  | Error err -> Result.Error ("Wrong connection error: " ^ Connection.error_to_string err)
  | Ok _ -> Result.Error "CONTINUATION on the wrong stream was accepted"

let test_process_data_accepts_split_header_block = fun _ctx ->
  let conn = create_server_connection () in
  let header_block = encode_header_block [ { Hpack.name = ":method"; value = "GET" }; ] in
  let headers = Frame.headers ~stream_id:1 ~end_headers:false "" in
  let continuation = Frame.continuation ~stream_id:1 ~end_headers:true header_block in
  let data = Frame.data ~stream_id:1 "ok" in
  match Connection.process_data
    conn
    (Std.IO.Bytes.from_string
      (serialize_frame headers ^ serialize_frame continuation ^ serialize_frame data)) with
  | Ok [
      Connection.HeadersReceived {
        stream_id = 1;
        headers = [ { Hpack.name = ":method"; value = "GET" } ];
        end_stream = false;
      };
      Connection.DataReceived { stream_id = 1; data; end_stream = false };
    ] when Std.IO.Bytes.to_string data = "ok" -> Result.Ok ()
  | Ok _ -> Result.Error "Split header block did not emit the expected headers and data events"
  | Error err -> Result.Error ("Split header block failed: " ^ Connection.error_to_string err)

let test_process_data_rejects_data_before_headers = fun _ctx ->
  let conn = create_server_connection () in
  let data = Frame.data ~stream_id:1 "hello" in
  match Connection.process_data conn (Std.IO.Bytes.from_string (serialize_frame data)) with
  | Error (Connection.DataBeforeHeaders { stream_id = 1 }) -> Result.Ok ()
  | Error err -> Result.Error ("Wrong connection error: " ^ Connection.error_to_string err)
  | Ok _ -> Result.Error "DATA before HEADERS was accepted"

let test_process_data_accepts_data_after_headers = fun _ctx ->
  let conn = create_server_connection () in
  let header_block = encode_header_block [ { Hpack.name = ":method"; value = "GET" }; ] in
  let headers = Frame.headers ~stream_id:1 ~end_headers:true header_block in
  let data = Frame.data ~stream_id:1 "hello" in
  match Connection.process_data
    conn
    (Std.IO.Bytes.from_string (serialize_frame headers ^ serialize_frame data)) with
  | Ok [
      Connection.HeadersReceived {
        stream_id = 1;
        headers = [ { Hpack.name = ":method"; value = "GET" } ];
        end_stream = false;
      };
      Connection.DataReceived { stream_id = 1; data; end_stream = false };
    ] when Std.IO.Bytes.to_string data = "hello" -> Result.Ok ()
  | Ok _ -> Result.Error "DATA after HEADERS did not emit the expected events"
  | Error err -> Result.Error ("DATA after HEADERS failed: " ^ Connection.error_to_string err)

let test_process_data_decrements_receive_windows = fun _ctx ->
  let config = { Connection.default_config with initial_window_size = 8 } in
  let conn = create_server_connection ~config () in
  let header_block = encode_header_block [ { Hpack.name = ":method"; value = "GET" }; ] in
  let headers = Frame.headers ~stream_id:1 ~end_headers:true header_block in
  let data = Frame.data ~stream_id:1 "abc" in
  match Connection.process_data
    conn
    (Std.IO.Bytes.from_string (serialize_frame headers ^ serialize_frame data)) with
  | Error err -> Result.Error ("DATA failed: " ^ Connection.error_to_string err)
  | Ok _ ->
      match Connection.receive_stream_window_size conn ~stream_id:1 with
      | None -> Result.Error "stream receive window was not created"
      | Some stream_window ->
          let connection_window = Connection.receive_connection_window_size conn in
          if not (Int.equal stream_window 5) then
            Result.Error ("stream receive window should be 5, got " ^ Int.to_string stream_window)
          else if not (Int.equal connection_window 65_532) then
            Result.Error ("connection receive window should be 65532, got "
            ^ Int.to_string connection_window)
          else
            Result.Ok ()

let test_process_data_rejects_stream_flow_control_excess = fun _ctx ->
  let config = { Connection.default_config with initial_window_size = 3 } in
  let conn = create_server_connection ~config () in
  let header_block = encode_header_block [ { Hpack.name = ":method"; value = "GET" }; ] in
  let headers = Frame.headers ~stream_id:1 ~end_headers:true header_block in
  let data = Frame.data ~stream_id:1 "abcd" in
  match Connection.process_data
    conn
    (Std.IO.Bytes.from_string (serialize_frame headers ^ serialize_frame data)) with
  | Error (
    Connection.FlowControlWindowExceeded {
      scope = Connection.StreamWindow { stream_id = 1 };
      data_size = 4;
      window_size = 3;
    }
  ) ->
      Result.Ok ()
  | Error err -> Result.Error ("Wrong connection error: " ^ Connection.error_to_string err)
  | Ok _ -> Result.Error "DATA beyond the stream receive window was accepted"

let test_process_data_rejects_even_peer_stream_on_server = fun _ctx ->
  let conn = create_server_connection () in
  let header_block = encode_header_block [ { Hpack.name = ":method"; value = "GET" }; ] in
  let headers = Frame.headers ~stream_id:2 ~end_headers:true header_block in
  match Connection.process_data conn (Std.IO.Bytes.from_string (serialize_frame headers)) with
  | Error (Connection.InvalidPeerStreamId { role = Connection.Server; stream_id = 2 }) ->
      Result.Ok ()
  | Error err -> Result.Error ("Wrong connection error: " ^ Connection.error_to_string err)
  | Ok _ -> Result.Error "Server accepted HEADERS on an even peer stream"

let test_process_data_rejects_lower_new_peer_stream = fun _ctx ->
  let conn = create_server_connection () in
  let header_block = encode_header_block [ { Hpack.name = ":method"; value = "GET" }; ] in
  let higher = Frame.headers ~stream_id:3 ~end_headers:true header_block in
  let lower = Frame.headers ~stream_id:1 ~end_headers:true header_block in
  match Connection.process_data conn (Std.IO.Bytes.from_string (serialize_frame higher)) with
  | Error err -> Result.Error ("Higher peer stream failed: " ^ Connection.error_to_string err)
  | Ok _ ->
      match Connection.process_data conn (Std.IO.Bytes.from_string (serialize_frame lower)) with
      | Error (Connection.PeerStreamIdNotIncreasing { stream_id = 1; last_stream_id = 3 }) ->
          Result.Ok ()
      | Error err -> Result.Error ("Wrong connection error: " ^ Connection.error_to_string err)
      | Ok _ -> Result.Error "Lower new peer stream was accepted after a higher stream"

let test_process_data_rejects_unknown_odd_stream_on_client = fun _ctx ->
  let conn = create_client_connection () in
  let header_block = encode_header_block [ { Hpack.name = ":status"; value = "200" }; ] in
  let headers = Frame.headers ~stream_id:1 ~end_headers:true header_block in
  match Connection.process_data conn (Std.IO.Bytes.from_string (serialize_frame headers)) with
  | Error (Connection.InvalidPeerStreamId { role = Connection.Client; stream_id = 1 }) ->
      Result.Ok ()
  | Error err -> Result.Error ("Wrong connection error: " ^ Connection.error_to_string err)
  | Ok _ -> Result.Error "Client accepted HEADERS on an unknown odd stream"

let test_process_data_accepts_response_on_existing_client_stream = fun _ctx ->
  let conn = create_client_connection () in
  match Connection.create_stream conn with
  | Error err -> Result.Error ("Failed to create client stream: " ^ Connection.error_to_string err)
  | Ok 1 ->
      let header_block = encode_header_block [ { Hpack.name = ":status"; value = "200" }; ] in
      let headers = Frame.headers ~stream_id:1 ~end_headers:true header_block in
      (
        match Connection.process_data conn (Std.IO.Bytes.from_string (serialize_frame headers)) with
        | Ok [
            Connection.HeadersReceived {
              stream_id = 1;
              headers = [ { Hpack.name = ":status"; value = "200" } ];
              end_stream = false;
            };
          ] -> Result.Ok ()
        | Ok _ -> Result.Error "Client response HEADERS emitted unexpected events"
        | Error err ->
            Result.Error ("Client response HEADERS failed: " ^ Connection.error_to_string err)
      )
  | Ok stream_id -> Result.Error ("Expected stream 1, got " ^ Int.to_string stream_id)

let test_process_data_rejects_peer_stream_over_max_concurrent = fun _ctx ->
  let config = { Connection.default_config with max_concurrent_streams = 1 } in
  let conn = create_server_connection ~config () in
  let header_block = encode_header_block [ { Hpack.name = ":method"; value = "GET" }; ] in
  let first_headers = Frame.headers ~stream_id:1 ~end_headers:true header_block in
  let second_headers = Frame.headers ~stream_id:3 ~end_headers:true header_block in
  match Connection.process_data conn (Std.IO.Bytes.from_string (serialize_frame first_headers)) with
  | Error err -> Result.Error ("First peer stream failed: " ^ Connection.error_to_string err)
  | Ok _ ->
      match Connection.process_data conn (Std.IO.Bytes.from_string (serialize_frame second_headers)) with
      | Error (
        Connection.MaxConcurrentStreamsExceeded {
          initiator = Connection.PeerInitiated;
          stream_id = 3;
          current = 1;
          limit = 1;
        }
      ) ->
          Result.Ok ()
      | Error err -> Result.Error ("Wrong connection error: " ^ Connection.error_to_string err)
      | Ok _ -> Result.Error "Peer stream over max concurrent streams was accepted"

let test_process_data_frees_peer_capacity_after_rst_stream = fun _ctx ->
  let config = { Connection.default_config with max_concurrent_streams = 1 } in
  let conn = create_server_connection ~config () in
  let header_block = encode_header_block [ { Hpack.name = ":method"; value = "GET" }; ] in
  let first_headers = Frame.headers ~stream_id:1 ~end_headers:true header_block in
  let rst_stream = Frame.rst_stream ~stream_id:1 Frame.Cancel in
  let second_headers = Frame.headers ~stream_id:3 ~end_headers:true header_block in
  match Connection.process_data
    conn
    (Std.IO.Bytes.from_string
      (serialize_frame first_headers ^ serialize_frame rst_stream ^ serialize_frame second_headers)) with
  | Ok [
      Connection.HeadersReceived { stream_id = 1; _ };
      Connection.RstStreamReceived { stream_id = 1; error_code = Frame.Cancel };
      Connection.HeadersReceived { stream_id = 3; _ };
    ] -> Result.Ok ()
  | Ok _ -> Result.Error "RST_STREAM did not free peer stream capacity as expected"
  | Error err ->
      Result.Error ("Peer stream after RST_STREAM failed: " ^ Connection.error_to_string err)

let test_create_stream_obeys_remote_max_concurrent = fun _ctx ->
  let conn = create_client_connection () in
  let remote_settings = Frame.settings [ Frame.MaxConcurrentStreams 1; ] in
  match Connection.process_data conn (Std.IO.Bytes.from_string (serialize_frame remote_settings)) with
  | Error err -> Result.Error ("Remote settings failed: " ^ Connection.error_to_string err)
  | Ok _ ->
      (match Connection.create_stream conn with
      | Error err -> Result.Error ("First local stream failed: " ^ Connection.error_to_string err)
      | Ok 1 ->
          (match Connection.create_stream conn with
          | Error (
            Connection.MaxConcurrentStreamsExceeded {
              initiator = Connection.LocalInitiated;
              stream_id = 3;
              current = 1;
              limit = 1;
            }
          ) ->
              Result.Ok ()
          | Error err -> Result.Error ("Wrong connection error: " ^ Connection.error_to_string err)
          | Ok _ -> Result.Error "Local stream over remote max concurrent streams was accepted")
      | Ok stream_id -> Result.Error ("Expected first stream 1, got " ^ Int.to_string stream_id))

let test_create_stream_uses_remote_initial_window = fun _ctx ->
  let conn = create_client_connection () in
  let remote_settings = Frame.settings [ Frame.InitialWindowSize 10; ] in
  match Connection.process_data conn (Std.IO.Bytes.from_string (serialize_frame remote_settings)) with
  | Error err -> Result.Error ("Remote settings failed: " ^ Connection.error_to_string err)
  | Ok _ ->
      match Connection.create_stream conn with
      | Error err -> Result.Error ("Creating stream failed: " ^ Connection.error_to_string err)
      | Ok stream_id ->
          match Connection.stream_window_size conn ~stream_id with
          | Some 10 -> Result.Ok ()
          | Some window ->
              Result.Error ("Expected stream send window 10, got " ^ Int.to_string window)
          | None -> Result.Error "New stream was missing"

let test_remote_initial_window_adjusts_existing_streams = fun _ctx ->
  let conn = create_client_connection () in
  match Connection.create_stream conn with
  | Error err -> Result.Error ("Creating stream failed: " ^ Connection.error_to_string err)
  | Ok stream_id ->
      match Connection.send_headers
        conn
        ~stream_id
        ~headers:[ { Hpack.name = ":method"; value = "GET" }; ]
        ~end_stream:false with
      | Error err -> Result.Error ("Sending HEADERS failed: " ^ Connection.error_to_string err)
      | Ok _ ->
          match Connection.send_data
            conn
            ~stream_id
            ~data:(Std.IO.Bytes.from_string "abc")
            ~end_stream:false with
          | Error err -> Result.Error ("Sending DATA failed: " ^ Connection.error_to_string err)
          | Ok _ ->
              let remote_settings = Frame.settings [ Frame.InitialWindowSize 10; ] in
              match Connection.process_data
                conn
                (Std.IO.Bytes.from_string (serialize_frame remote_settings)) with
              | Error err ->
                  Result.Error ("Remote settings failed: " ^ Connection.error_to_string err)
              | Ok _ ->
                  match Connection.stream_window_size conn ~stream_id with
                  | Some 7 -> Result.Ok ()
                  | Some window ->
                      Result.Error ("Expected adjusted stream send window 7, got "
                      ^ Int.to_string window)
                  | None -> Result.Error "Existing stream was missing"

let test_process_data_rejects_new_stream_after_goaway = fun _ctx ->
  let conn = create_server_connection () in
  let goaway = Frame.goaway ~last_stream_id:1 ~error_code:Frame.NoError () in
  let header_block = encode_header_block [ { Hpack.name = ":method"; value = "GET" }; ] in
  let headers = Frame.headers ~stream_id:3 ~end_headers:true header_block in
  match Connection.process_data
    conn
    (Std.IO.Bytes.from_string (serialize_frame goaway ^ serialize_frame headers)) with
  | Error (Connection.NewStreamRejected { state = Connection.GoingAway; stream_id = 3 }) ->
      Result.Ok ()
  | Error err -> Result.Error ("Wrong connection error: " ^ Connection.error_to_string err)
  | Ok _ -> Result.Error "New stream after GOAWAY was accepted"

let test_process_data_allows_existing_stream_after_goaway = fun _ctx ->
  let conn = create_server_connection () in
  let header_block = encode_header_block [ { Hpack.name = ":method"; value = "GET" }; ] in
  let headers = Frame.headers ~stream_id:1 ~end_headers:true header_block in
  let goaway = Frame.goaway ~last_stream_id:1 ~error_code:Frame.NoError () in
  let data = Frame.data ~stream_id:1 "ok" in
  match Connection.process_data
    conn
    (Std.IO.Bytes.from_string
      (serialize_frame headers ^ serialize_frame goaway ^ serialize_frame data)) with
  | Ok [
      Connection.HeadersReceived { stream_id = 1; _ };
      Connection.GoawayReceived { last_stream_id = 1; error_code = Frame.NoError; _ };
      Connection.DataReceived { stream_id = 1; data; _ };
    ] when Std.IO.Bytes.to_string data = "ok" -> Result.Ok ()
  | Ok _ -> Result.Error "Existing stream after GOAWAY emitted unexpected events"
  | Error err ->
      Result.Error ("Existing stream after GOAWAY failed: " ^ Connection.error_to_string err)

let test_process_window_update_increases_send_window_only = fun _ctx ->
  let conn = create_server_connection () in
  let send_before = Connection.connection_window_size conn in
  let receive_before = Connection.receive_connection_window_size conn in
  let frame = Frame.window_update ~stream_id:0 10 in
  match frame with
  | Error error ->
      Result.Error ("WINDOW_UPDATE construction failed: " ^ Frame.constructor_error_to_string error)
  | Ok frame ->
      match Connection.process_data conn (Std.IO.Bytes.from_string (serialize_frame frame)) with
      | Error err -> Result.Error ("WINDOW_UPDATE failed: " ^ Connection.error_to_string err)
      | Ok [ Connection.WindowUpdateReceived { stream_id = 0; increment = 10 } ] ->
          let send_after = Connection.connection_window_size conn in
          let receive_after = Connection.receive_connection_window_size conn in
          if not (Int.equal send_after (send_before + 10)) then
            Result.Error "received WINDOW_UPDATE did not increase the outbound send window"
          else if not (Int.equal receive_after receive_before) then
            Result.Error "received WINDOW_UPDATE changed the receive window"
          else
            Result.Ok ()
      | Ok _ -> Result.Error "WINDOW_UPDATE produced unexpected events"

let test_process_data_rejects_connection_window_overflow = fun _ctx ->
  let conn = create_server_connection () in
  match Frame.window_update ~stream_id:0 2_147_483_647 with
  | Error error ->
      Result.Error ("WINDOW_UPDATE construction failed: " ^ Frame.constructor_error_to_string error)
  | Ok frame ->
      match Connection.process_data conn (Std.IO.Bytes.from_string (serialize_frame frame)) with
      | Error (
        Connection.FlowControlWindowOverflow {
          scope = Connection.ConnectionWindow;
          increment = 2_147_483_647;
          window_size = 65_535;
          max_size = 2_147_483_647;
        }
      ) ->
          Result.Ok ()
      | Error err -> Result.Error ("Wrong connection error: " ^ Connection.error_to_string err)
      | Ok _ -> Result.Error "WINDOW_UPDATE overflow was accepted"

let test_process_data_rejects_stream_window_overflow = fun _ctx ->
  let conn = create_server_connection () in
  let header_block = encode_header_block [ { Hpack.name = ":method"; value = "GET" }; ] in
  let headers = Frame.headers ~stream_id:1 ~end_headers:true header_block in
  match Frame.window_update ~stream_id:1 2_147_483_647 with
  | Error error ->
      Result.Error ("WINDOW_UPDATE construction failed: " ^ Frame.constructor_error_to_string error)
  | Ok window_update ->
      match Connection.process_data
        conn
        (Std.IO.Bytes.from_string (serialize_frame headers ^ serialize_frame window_update)) with
      | Error (
        Connection.FlowControlWindowOverflow {
          scope = Connection.StreamWindow { stream_id = 1 };
          increment = 2_147_483_647;
          window_size = 65_535;
          max_size = 2_147_483_647;
        }
      ) ->
          Result.Ok ()
      | Error err -> Result.Error ("Wrong connection error: " ^ Connection.error_to_string err)
      | Ok _ -> Result.Error "stream WINDOW_UPDATE overflow was accepted"

let test_process_data_rejects_window_update_for_idle_stream = fun _ctx ->
  let conn = create_server_connection () in
  match Frame.window_update ~stream_id:1 10 with
  | Error error ->
      Result.Error ("WINDOW_UPDATE construction failed: " ^ Frame.constructor_error_to_string error)
  | Ok frame ->
      match Connection.process_data conn (Std.IO.Bytes.from_string (serialize_frame frame)) with
      | Error (Connection.FrameForIdleStream { stream_id = 1; frame_type = Frame.WindowUpdate }) ->
          Result.Ok ()
      | Error err -> Result.Error ("Wrong connection error: " ^ Connection.error_to_string err)
      | Ok _ -> Result.Error "WINDOW_UPDATE for idle stream was accepted"

let test_process_data_rejects_rst_stream_for_idle_stream = fun _ctx ->
  let conn = create_server_connection () in
  let frame = Frame.rst_stream ~stream_id:1 Frame.Cancel in
  match Connection.process_data conn (Std.IO.Bytes.from_string (serialize_frame frame)) with
  | Error (Connection.FrameForIdleStream { stream_id = 1; frame_type = Frame.RstStream }) ->
      Result.Ok ()
  | Error err -> Result.Error ("Wrong connection error: " ^ Connection.error_to_string err)
  | Ok _ -> Result.Error "RST_STREAM for idle stream was accepted"

let test_process_data_rejects_data_after_headers_end_stream = fun _ctx ->
  let conn = create_server_connection () in
  let header_block = encode_header_block [ { Hpack.name = ":method"; value = "GET" }; ] in
  let headers = Frame.headers ~stream_id:1 ~end_headers:true ~end_stream:true header_block in
  let data = Frame.data ~stream_id:1 "late" in
  match Connection.process_data
    conn
    (Std.IO.Bytes.from_string (serialize_frame headers ^ serialize_frame data)) with
  | Error (
    Connection.FrameAfterStreamEnd {
      stream_id = 1;
      frame_type = Frame.Data;
      state = Connection.StreamHalfClosedRemote;
    }
  ) ->
      Result.Ok ()
  | Error err -> Result.Error ("Wrong connection error: " ^ Connection.error_to_string err)
  | Ok _ -> Result.Error "DATA after end-stream HEADERS was accepted"

let test_process_data_rejects_data_after_data_end_stream = fun _ctx ->
  let conn = create_server_connection () in
  let header_block = encode_header_block [ { Hpack.name = ":method"; value = "GET" }; ] in
  let headers = Frame.headers ~stream_id:1 ~end_headers:true header_block in
  let end_data = Frame.data ~stream_id:1 ~end_stream:true "done" in
  let late_data = Frame.data ~stream_id:1 "late" in
  match Connection.process_data
    conn
    (Std.IO.Bytes.from_string
      (serialize_frame headers ^ serialize_frame end_data ^ serialize_frame late_data)) with
  | Error (
    Connection.FrameAfterStreamEnd {
      stream_id = 1;
      frame_type = Frame.Data;
      state = Connection.StreamHalfClosedRemote;
    }
  ) ->
      Result.Ok ()
  | Error err -> Result.Error ("Wrong connection error: " ^ Connection.error_to_string err)
  | Ok _ -> Result.Error "DATA after end-stream DATA was accepted"

let test_process_data_rejects_headers_after_rst_stream = fun _ctx ->
  let conn = create_server_connection () in
  let header_block = encode_header_block [ { Hpack.name = ":method"; value = "GET" }; ] in
  let headers = Frame.headers ~stream_id:1 ~end_headers:true header_block in
  let rst_stream = Frame.rst_stream ~stream_id:1 Frame.Cancel in
  let late_headers = Frame.headers ~stream_id:1 ~end_headers:true header_block in
  match Connection.process_data
    conn
    (Std.IO.Bytes.from_string
      (serialize_frame headers ^ serialize_frame rst_stream ^ serialize_frame late_headers)) with
  | Error (
    Connection.FrameAfterStreamEnd {
      stream_id = 1;
      frame_type = Frame.Headers;
      state = Connection.StreamClosed;
    }
  ) ->
      Result.Ok ()
  | Error err -> Result.Error ("Wrong connection error: " ^ Connection.error_to_string err)
  | Ok _ -> Result.Error "HEADERS after RST_STREAM was accepted"

let test_process_data_rejects_headers_after_headers_end_stream = fun _ctx ->
  let conn = create_server_connection () in
  let header_block = encode_header_block [ { Hpack.name = ":method"; value = "GET" }; ] in
  let headers = Frame.headers ~stream_id:1 ~end_headers:true ~end_stream:true header_block in
  let late_headers = Frame.headers ~stream_id:1 ~end_headers:true header_block in
  match Connection.process_data
    conn
    (Std.IO.Bytes.from_string (serialize_frame headers ^ serialize_frame late_headers)) with
  | Error (
    Connection.FrameAfterStreamEnd {
      stream_id = 1;
      frame_type = Frame.Headers;
      state = Connection.StreamHalfClosedRemote;
    }
  ) ->
      Result.Ok ()
  | Error err -> Result.Error ("Wrong connection error: " ^ Connection.error_to_string err)
  | Ok _ -> Result.Error "HEADERS after end-stream HEADERS was accepted"

let tests =
  Test.[
    case "serialize_settings_frame" test_serialize_settings_frame;
    case "serialize_data_frame" test_serialize_data_frame;
    case "serialize_recomputes_payload_length" test_serialize_recomputes_payload_length;
    case "serialize_settings_payload_length" test_serialize_settings_payload_length;
    case "serialize_settings_ack_has_empty_payload" test_serialize_settings_ack_has_empty_payload;
    case "serialize_settings_ack_rejects_payload" test_serialize_settings_ack_rejects_payload;
    case
      "serialize_rejects_invalid_initial_window_setting"
      test_serialize_rejects_invalid_initial_window_setting;
    case
      "serialize_rejects_invalid_max_frame_size_setting"
      test_serialize_rejects_invalid_max_frame_size_setting;
    case "serialize_rejects_negative_uint32_setting" test_serialize_rejects_negative_uint32_setting;
    case "serialize_rejects_payload_mismatch" test_serialize_rejects_payload_mismatch;
    case
      "serialize_rejects_invalid_ping_payload_length"
      test_serialize_rejects_invalid_ping_payload_length;
    case
      "serialize_rejects_invalid_window_update_increment"
      test_serialize_rejects_invalid_window_update_increment;
    case "serialize_rejects_payload_length_overflow" test_serialize_rejects_payload_length_overflow;
    case
      "parse_frame_respects_configured_max_frame_size"
      test_parse_frame_respects_configured_max_frame_size;
    case
      "parse_frame_rejects_over_configured_max_frame_size"
      test_parse_frame_rejects_over_configured_max_frame_size;
    case
      "serialize_rejects_invalid_unknown_frame_type_code"
      test_serialize_rejects_invalid_unknown_frame_type_code;
    case "serialize_rejects_zero_stream_data_frame" test_serialize_rejects_zero_stream_data_frame;
    case
      "serialize_rejects_nonzero_stream_settings_frame"
      test_serialize_rejects_nonzero_stream_settings_frame;
    case "serialize_rejects_invalid_padding_length" test_serialize_rejects_invalid_padding_length;
    case "serialize_rejects_invalid_priority_weight" test_serialize_rejects_invalid_priority_weight;
    case
      "serialize_rejects_invalid_stream_dependency"
      test_serialize_rejects_invalid_stream_dependency;
    case
      "serialize_rejects_self_priority_dependency"
      test_serialize_rejects_self_priority_dependency;
    case
      "serialize_rejects_self_headers_priority_dependency"
      test_serialize_rejects_self_headers_priority_dependency;
    case
      "serialize_rejects_incomplete_headers_priority"
      test_serialize_rejects_incomplete_headers_priority;
    case "serialize_rejects_negative_stream_id" test_serialize_rejects_negative_stream_id;
    case
      "serialize_rejects_invalid_promised_stream_id"
      test_serialize_rejects_invalid_promised_stream_id;
    case "serialize_rejects_invalid_last_stream_id" test_serialize_rejects_invalid_last_stream_id;
    case "serialize_preserves_unknown_error_code" test_serialize_preserves_unknown_error_code;
    case
      "serialize_rejects_invalid_unknown_error_code"
      test_serialize_rejects_invalid_unknown_error_code;
    case "ping_rejects_invalid_payload_length" test_ping_rejects_invalid_payload_length;
    case "window_update_rejects_invalid_increment" test_window_update_rejects_invalid_increment;
    case
      "connection_window_update_invalid_increment_preserves_state"
      test_connection_window_update_invalid_increment_preserves_state;
    case
      "connection_window_update_increases_receive_window_only"
      test_connection_window_update_increases_receive_window_only;
    case "send_data_splits_by_remote_max_frame_size" test_send_data_splits_by_remote_max_frame_size;
    case
      "send_headers_splits_continuations_by_remote_max_frame_size"
      test_send_headers_splits_continuations_by_remote_max_frame_size;
    case "client_preface_settings_payload_length" test_client_preface_settings_payload_length;
    case "server_preface_settings_payload_length" test_server_preface_settings_payload_length;
    case "server_preface_disables_push_by_default" test_server_preface_disables_push_by_default;
    case "server_accepts_split_client_preface" test_server_accepts_split_client_preface;
    case "server_rejects_malformed_client_preface" test_server_rejects_malformed_client_preface;
    case "client_rejects_non_settings_initial_frame" test_client_rejects_non_settings_initial_frame;
    case "process_data_buffers_split_frame_header" test_process_data_buffers_split_frame_header;
    case "process_data_buffers_split_frame_payload" test_process_data_buffers_split_frame_payload;
    case
      "process_data_buffers_frame_one_byte_at_a_time"
      test_process_data_buffers_frame_one_byte_at_a_time;
    case
      "process_data_buffers_split_continuation_payload"
      test_process_data_buffers_split_continuation_payload;
    case
      "process_data_clears_pending_input_after_parse_error"
      test_process_data_clears_pending_input_after_parse_error;
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
    case "parse_data_rejects_padded_zero_length" test_parse_data_rejects_padded_zero_length;
    case "parse_headers_rejects_padded_zero_length" test_parse_headers_rejects_padded_zero_length;
    case
      "parse_headers_rejects_short_priority_payload"
      test_parse_headers_rejects_short_priority_payload;
    case
      "parse_push_promise_rejects_short_promised_stream"
      test_parse_push_promise_rejects_short_promised_stream;
    case "parse_priority_rejects_self_dependency" test_parse_priority_rejects_self_dependency;
    case
      "parse_headers_rejects_self_priority_dependency"
      test_parse_headers_rejects_self_priority_dependency;
    case
      "parse_rst_stream_preserves_unknown_error_code"
      test_parse_rst_stream_preserves_unknown_error_code;
    case "parse_goaway_preserves_unknown_error_code" test_parse_goaway_preserves_unknown_error_code;
    case "parse_window_update_allows_stream_zero" test_parse_window_update_allows_stream_zero;
    case "parse_unknown_frame_preserves_payload" test_parse_unknown_frame_preserves_payload;
    case "process_data_ignores_unknown_frame" test_process_data_ignores_unknown_frame;
    case "process_data_rejects_push_promise" test_process_data_rejects_push_promise;
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
    case "process_data_decrements_receive_windows" test_process_data_decrements_receive_windows;
    case
      "process_data_rejects_stream_flow_control_excess"
      test_process_data_rejects_stream_flow_control_excess;
    case
      "process_data_rejects_even_peer_stream_on_server"
      test_process_data_rejects_even_peer_stream_on_server;
    case
      "process_data_rejects_lower_new_peer_stream"
      test_process_data_rejects_lower_new_peer_stream;
    case
      "process_data_rejects_unknown_odd_stream_on_client"
      test_process_data_rejects_unknown_odd_stream_on_client;
    case
      "process_data_accepts_response_on_existing_client_stream"
      test_process_data_accepts_response_on_existing_client_stream;
    case
      "process_data_rejects_peer_stream_over_max_concurrent"
      test_process_data_rejects_peer_stream_over_max_concurrent;
    case
      "process_data_frees_peer_capacity_after_rst_stream"
      test_process_data_frees_peer_capacity_after_rst_stream;
    case "create_stream_obeys_remote_max_concurrent" test_create_stream_obeys_remote_max_concurrent;
    case "create_stream_uses_remote_initial_window" test_create_stream_uses_remote_initial_window;
    case
      "remote_initial_window_adjusts_existing_streams"
      test_remote_initial_window_adjusts_existing_streams;
    case
      "process_data_rejects_new_stream_after_goaway"
      test_process_data_rejects_new_stream_after_goaway;
    case
      "process_data_allows_existing_stream_after_goaway"
      test_process_data_allows_existing_stream_after_goaway;
    case
      "process_window_update_increases_send_window_only"
      test_process_window_update_increases_send_window_only;
    case
      "process_data_rejects_connection_window_overflow"
      test_process_data_rejects_connection_window_overflow;
    case
      "process_data_rejects_stream_window_overflow"
      test_process_data_rejects_stream_window_overflow;
    case
      "process_data_rejects_window_update_for_idle_stream"
      test_process_data_rejects_window_update_for_idle_stream;
    case
      "process_data_rejects_rst_stream_for_idle_stream"
      test_process_data_rejects_rst_stream_for_idle_stream;
    case
      "process_data_rejects_data_after_headers_end_stream"
      test_process_data_rejects_data_after_headers_end_stream;
    case
      "process_data_rejects_data_after_data_end_stream"
      test_process_data_rejects_data_after_data_end_stream;
    case
      "process_data_rejects_headers_after_rst_stream"
      test_process_data_rejects_headers_after_rst_stream;
    case
      "process_data_rejects_headers_after_headers_end_stream"
      test_process_data_rejects_headers_after_headers_end_stream;
  ]

let main ~args:_ = Test.Cli.main ~name:"http:http2_parser" ~tests ~args:Env.args ()

let () = Runtime.run ~main ~args:Env.args ()
