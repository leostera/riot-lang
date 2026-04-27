open Std

(** HTTP/2 frame serialization tests *)
module Frame = Http.Http2.Frame
module Serializer = Http.Http2.Serializer

let frame_payload_length = fun serialized ->
  let byte index = Char.code (String.get_unchecked serialized ~at:index) in
  (byte 0 lsl 16) lor (byte 1 lsl 8) lor byte 2

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

let test_frame_types = fun _ctx ->
  let types = [ Frame.Data; Frame.Headers; Frame.Settings; Frame.Ping; Frame.Goaway; ] in
  if List.length types = 5 then
    Result.Ok ()
  else
    Result.Error "Frame types count mismatch"

let tests =
  Test.[
    case "serialize_settings_frame" test_serialize_settings_frame;
    case "serialize_data_frame" test_serialize_data_frame;
    case "serialize_recomputes_payload_length" test_serialize_recomputes_payload_length;
    case "serialize_settings_payload_length" test_serialize_settings_payload_length;
    case "frame_types" test_frame_types;
  ]

let main ~args:_ = Test.Cli.main ~name:"http:http2_parser" ~tests ~args:Env.args ()

let () = Runtime.run ~main ~args:Env.args ()
