open Std

module Json = Data.Json
module Http1 = Http.Http1
module Http2 = Http.Http2
module Ws = Http.Ws
module Request = Std.Net.Http.Request
module Response = Std.Net.Http.Response
module Header = Std.Net.Http.Header
module Method = Std.Net.Http.Method
module Version = Std.Net.Http.Version
module Status = Std.Net.Http.Status
module Uri = Std.Net.Uri
module Body = Std.Net.Http.Body

let fixture_root = Path.v "packages/http/tests/fixtures"

let ( let* ) = fun result fn -> Result.and_then result ~fn

let keep_fixture = fun path ->
  match Path.extension path with
  | Some ".http"
  | Some ".frame"
  | Some ".txt" -> Test.FixtureRunner.Keep
  | _ -> Test.FixtureRunner.Skip

let replace_suffix = fun value ~suffix ~replacement ->
  if String.ends_with ~suffix value then
    let prefix_len = String.length value - String.length suffix in
    Some (String.sub value ~offset:0 ~len:prefix_len ^ replacement)
  else
    None

let expected_path = fun fixture_path ->
  let path = Path.to_string fixture_path in
  let expected =
    replace_suffix path ~suffix:".http" ~replacement:".expected"
    |> Option.or_else ~fn:(fun () -> replace_suffix path ~suffix:".frame" ~replacement:".expected")
    |> Option.or_else ~fn:(fun () -> replace_suffix path ~suffix:".txt" ~replacement:".expected")
  in
  match expected with
  | Some path -> Ok (Path.v path)
  | None -> Error ("unsupported fixture extension: " ^ path)

let read_file = fun path ->
  Fs.read path
  |> Result.map_err ~fn:IO.error_message

let read_expected_json = fun fixture_path ->
  let* expected_path = expected_path fixture_path in
  let* source = read_file expected_path in
  Json.from_string source
  |> Result.map_err ~fn:Json.error_to_string

let json_of_option = fun to_json value ->
  match value with
  | None -> Json.null
  | Some value -> to_json value

let headers_json = fun headers ->
  let rec loop seen acc = fun __tmp1 ->
    match __tmp1 with
    | [] -> Json.obj (List.reverse acc)
    | (name, value) :: rest ->
        let normalized = String.lowercase_ascii name in
        if List.contains seen ~value:normalized then
          loop seen acc rest
        else
          loop (normalized :: seen) ((name, Json.string value) :: acc) rest
  in
  loop [] [] headers

let request_target = fun uri ->
  match Uri.scheme uri with
  | Some _ -> Uri.to_string uri
  | None -> Uri.path_and_query uri

let body_string = fun body ->
  match body with
  | None -> ""
  | Some body -> Body.to_string body

let request_json = fun request ->
  Json.obj
    [
      ("method", Json.string
        (
          Request.method_ request
          |> Method.to_string
        ));
      ("path", Json.string
        (
          Request.uri request
          |> request_target
        ));
      ("version", Json.string
        (
          Request.version request
          |> Version.to_string
        ));
      ("headers", Header.to_list (Request.headers request)
      |> headers_json);
      ("body", Json.string
        (
          Request.body request
          |> body_string
        ));
    ]

let response_json = fun response ->
  let status = Response.status response in
  Json.obj
    [
      ("version", Json.string
        (
          Response.version response
          |> Version.to_string
        ));
      ("status_code", Json.int (Status.to_int status));
      ("reason", Json.string (Status.reason_phrase status));
      ("headers", Header.to_list (Response.headers response)
      |> headers_json);
      ("body", Json.string
        (
          Response.body response
          |> body_string
        ));
    ]

let http1_header_format_error_json = fun error ->
  match error with
  | Http1.Common.MissingColon -> Json.obj [ ("type", Json.string "MissingColon") ]
  | Http1.Common.MissingValueSeparator -> Json.obj [ ("type", Json.string "MissingValueSeparator") ]
  | Http1.Common.EmptyName -> Json.obj [ ("type", Json.string "EmptyName") ]
  | Http1.Common.WhitespaceBeforeColon -> Json.obj [ ("type", Json.string "WhitespaceBeforeColon") ]
  | Http1.Common.ObsoleteLineFolding -> Json.obj [ ("type", Json.string "ObsoleteLineFolding") ]
  | Http1.Common.InvalidNameCharacter { code; index } ->
      Json.obj
        [
          ("type", Json.string "InvalidNameCharacter");
          ("code", Json.int code);
          ("index", Json.int index);
        ]
  | Http1.Common.InvalidValueCharacter { code; index } ->
      Json.obj
        [
          ("type", Json.string "InvalidValueCharacter");
          ("code", Json.int code);
          ("index", Json.int index);
        ]

let http1_content_length_error_json = fun error ->
  match error with
  | Http1.Common.EmptyContentLength -> Json.obj [ ("type", Json.string "EmptyContentLength") ]
  | Http1.Common.NegativeContentLength -> Json.obj [ ("type", Json.string "NegativeContentLength") ]
  | Http1.Common.ContentLengthOverflow -> Json.obj [ ("type", Json.string "ContentLengthOverflow") ]
  | Http1.Common.InvalidContentLengthCharacter { code; index } ->
      Json.obj
        [
          ("type", Json.string "InvalidContentLengthCharacter");
          ("code", Json.int code);
          ("index", Json.int index);
        ]

let http1_chunk_size_error_json = fun error ->
  match error with
  | Http1.Common.EmptyChunkSize -> Json.obj [ ("type", Json.string "EmptyChunkSize") ]
  | Http1.Common.ChunkSizeOverflow -> Json.obj [ ("type", Json.string "ChunkSizeOverflow") ]
  | Http1.Common.InvalidChunkSizeCharacter { code; index } ->
      Json.obj
        [
          ("type", Json.string "InvalidChunkSizeCharacter");
          ("code", Json.int code);
          ("index", Json.int index);
        ]

let http1_status_code_error_json = fun error ->
  match error with
  | Http1.Common.StatusCodeLength { length; expected } ->
      Json.obj
        [
          ("type", Json.string "StatusCodeLength");
          ("length", Json.int length);
          ("expected", Json.int expected);
        ]
  | Http1.Common.InvalidStatusCodeCharacter { code; index } ->
      Json.obj
        [
          ("type", Json.string "InvalidStatusCodeCharacter");
          ("code", Json.int code);
          ("index", Json.int index);
        ]
  | Http1.Common.StatusCodeOutOfRange { code; min; max } ->
      Json.obj
        [
          ("type", Json.string "StatusCodeOutOfRange");
          ("code", Json.int code);
          ("min", Json.int min);
          ("max", Json.int max);
        ]

let http1_error_json = fun error ->
  match error with
  | Http1.Common.InvalidCrlf -> Json.obj [ ("type", Json.string "InvalidCrlf") ]
  | Http1.Common.RequestLineTooLong { max_length } ->
      Json.obj [ ("type", Json.string "RequestLineTooLong"); ("max_length", Json.int max_length); ]
  | Http1.Common.StatusLineTooLong { max_length } ->
      Json.obj [ ("type", Json.string "StatusLineTooLong"); ("max_length", Json.int max_length); ]
  | Http1.Common.MissingMethod -> Json.obj [ ("type", Json.string "MissingMethod") ]
  | Http1.Common.MissingPath -> Json.obj [ ("type", Json.string "MissingPath") ]
  | Http1.Common.InvalidHttpVersion -> Json.obj [ ("type", Json.string "InvalidHttpVersion") ]
  | Http1.Common.InvalidRequestTarget _ -> Json.obj [ ("type", Json.string "InvalidRequestTarget") ]
  | Http1.Common.MissingVersion -> Json.obj [ ("type", Json.string "MissingVersion") ]
  | Http1.Common.MissingStatusCode -> Json.obj [ ("type", Json.string "MissingStatusCode") ]
  | Http1.Common.InvalidStatusCode reason ->
      Json.obj
        [
          ("type", Json.string "InvalidStatusCode");
          ("reason", http1_status_code_error_json reason);
        ]
  | Http1.Common.InvalidHeaderFormat reason ->
      Json.obj
        [
          ("type", Json.string "InvalidHeaderFormat");
          ("reason", http1_header_format_error_json reason);
        ]
  | Http1.Common.HeaderTooLong { max_length } ->
      Json.obj [ ("type", Json.string "HeaderTooLong"); ("max_length", Json.int max_length); ]
  | Http1.Common.HeaderBlockTooLong { max_length } ->
      Json.obj [ ("type", Json.string "HeaderBlockTooLong"); ("max_length", Json.int max_length); ]
  | Http1.Common.TooManyHeaders { max_count } ->
      Json.obj [ ("type", Json.string "TooManyHeaders"); ("max_count", Json.int max_count); ]
  | Http1.Common.InvalidContentLength reason ->
      Json.obj
        [
          ("type", Json.string "InvalidContentLength");
          ("reason", http1_content_length_error_json reason);
        ]
  | Http1.Common.ConflictingContentLength { expected; actual } ->
      Json.obj
        [
          ("type", Json.string "ConflictingContentLength");
          ("expected", Json.int expected);
          ("actual", Json.int actual);
        ]
  | Http1.Common.BodyTooLarge { size; max_size } ->
      Json.obj
        [
          ("type", Json.string "BodyTooLarge");
          ("size", Json.int size);
          ("max_size", Json.int max_size);
        ]
  | Http1.Common.UnsupportedTransferEncoding ->
      Json.obj [ ("type", Json.string "UnsupportedTransferEncoding") ]
  | Http1.Common.TransferEncodingWithContentLength ->
      Json.obj [ ("type", Json.string "TransferEncodingWithContentLength") ]
  | Http1.Common.InputSliceCreationFailed _ ->
      Json.obj [ ("type", Json.string "InputSliceCreationFailed") ]
  | Http1.Common.InvalidChunkSizeLineEnding ->
      Json.obj [ ("type", Json.string "InvalidChunkSizeLineEnding") ]
  | Http1.Common.InvalidChunkDataLineEnding ->
      Json.obj [ ("type", Json.string "InvalidChunkDataLineEnding") ]
  | Http1.Common.ChunkSizeLineTooLong { max_length } ->
      Json.obj
        [ ("type", Json.string "ChunkSizeLineTooLong"); ("max_length", Json.int max_length); ]
  | Http1.Common.InvalidChunkSize reason ->
      Json.obj
        [
          ("type", Json.string "InvalidChunkSize");
          ("reason", http1_chunk_size_error_json reason);
        ]
  | Http1.Common.InvalidChunkExtensionCharacter { code; index } ->
      Json.obj
        [
          ("type", Json.string "InvalidChunkExtensionCharacter");
          ("code", Json.int code);
          ("index", Json.int index);
        ]
  | Http1.Common.ChunkTooLarge { size; max_size } ->
      Json.obj
        [
          ("type", Json.string "ChunkTooLarge");
          ("size", Json.int size);
          ("max_size", Json.int max_size);
        ]
  | Http1.Common.ChunkedBodyTooLarge { size; max_size } ->
      Json.obj
        [
          ("type", Json.string "ChunkedBodyTooLarge");
          ("size", Json.int size);
          ("max_size", Json.int max_size);
        ]

let find_header_end = fun input -> Http1.Common.find_substring ~needle:"\r\n\r\n" input

let parse_header_lines = fun lines ->
  let rec loop acc = fun __tmp1 ->
    match __tmp1 with
    | [] -> Ok (List.reverse acc)
    | line :: rest ->
        if String.equal line "" then
          loop acc rest
        else
          match String.index_of line ~char:':' with
          | None -> Error ("invalid fixture header line: " ^ line)
          | Some colon ->
              let name = String.sub line ~offset:0 ~len:colon in
              let value =
                String.sub line ~offset:(colon + 1) ~len:(String.length line - colon - 1)
                |> String.trim
              in
              loop ((name, value) :: acc) rest
  in
  loop [] lines

let fixture_request_json = fun input ->
  match Http1.Request.parse input with
  | Http1.Common.Done { value; _ } -> Ok (request_json value)
  | Http1.Common.Error error -> Error (Http1.Common.error_to_string error)
  | Http1.Common.Need_more ->
      match find_header_end input with
      | None -> Error "incomplete HTTP/1 request fixture"
      | Some header_end ->
          let header_block = String.sub input ~offset:0 ~len:header_end in
          let body =
            String.sub input ~offset:(header_end + 4) ~len:(String.length input - header_end - 4)
          in
          match String.split ~by:"\r\n" header_block with
          | request_line :: header_lines ->
              match String.split ~by:" " request_line with
              | method_ :: target :: version_parts ->
                  let version = String.concat " " version_parts in
                  let* headers = parse_header_lines header_lines in
                  Ok (Json.obj
                    [
                      ("method", Json.string method_);
                      ("path", Json.string target);
                      ("version", Json.string version);
                      ("headers", headers_json headers);
                      ("body", Json.string body);
                    ])
              | _ -> Error ("invalid HTTP/1 request line fixture: " ^ request_line)
          | [] -> Error "empty HTTP/1 request fixture"

let fixture_response_json = fun input ->
  match Http1.Response.parse input with
  | Http1.Common.Done { value; _ } -> Ok (response_json value)
  | Http1.Common.Need_more -> Error "incomplete HTTP/1 response fixture"
  | Http1.Common.Error error -> Error (Http1.Common.error_to_string error)

let fixture_request_error_json = fun input ->
  match Http1.Request.parse ~max_request_line:8 input with
  | Http1.Common.Error error -> Ok (Json.obj [ ("error", http1_error_json error) ])
  | Http1.Common.Need_more -> Error "HTTP/1 request error fixture needed more data"
  | Http1.Common.Done _ -> Error "HTTP/1 request error fixture parsed successfully"

let fixture_response_error_json = fun input ->
  match Http1.Response.parse ~max_status_line:8 input with
  | Http1.Common.Error error -> Ok (Json.obj [ ("error", http1_error_json error) ])
  | Http1.Common.Need_more -> Error "HTTP/1 response error fixture needed more data"
  | Http1.Common.Done _ -> Error "HTTP/1 response error fixture parsed successfully"

let hex_digit = fun value -> String.get_unchecked "0123456789abcdef" ~at:value

let hex_string = fun value ->
  let len = String.length value in
  let out = IO.Bytes.create ~size:(len * 2) in
  for idx = 0 to len - 1 do
    let byte =
      value
      |> String.get_unchecked ~at:idx
      |> Char.to_int
    in
    IO.Bytes.set_unchecked out ~at:(idx * 2) ~char:(hex_digit (byte lsr 4));
    IO.Bytes.set_unchecked out ~at:(idx * 2 + 1) ~char:(hex_digit (byte land 0x0f))
  done;
  IO.Bytes.to_string out

let frame_type_string = fun __tmp1 ->
  match __tmp1 with
  | Http2.Frame.Data -> "Data"
  | Http2.Frame.Headers -> "Headers"
  | Http2.Frame.Priority -> "Priority"
  | Http2.Frame.RstStream -> "RstStream"
  | Http2.Frame.Settings -> "Settings"
  | Http2.Frame.PushPromise -> "PushPromise"
  | Http2.Frame.Ping -> "Ping"
  | Http2.Frame.Goaway -> "Goaway"
  | Http2.Frame.WindowUpdate -> "WindowUpdate"
  | Http2.Frame.Continuation -> "Continuation"
  | Http2.Frame.Unknown code -> "Unknown(" ^ Int.to_string code ^ ")"

let flags_json = fun flags ->
  Json.obj
    [
      ("end_stream", Json.bool flags.Http2.Frame.end_stream);
      ("end_headers", Json.bool flags.Http2.Frame.end_headers);
      ("padded", Json.bool flags.Http2.Frame.padded);
      ("priority", Json.bool flags.Http2.Frame.priority);
      ("ack", Json.bool flags.Http2.Frame.ack);
    ]

let error_code_string = fun __tmp1 ->
  match __tmp1 with
  | Http2.Frame.NoError -> "NO_ERROR"
  | Http2.Frame.ProtocolError -> "PROTOCOL_ERROR"
  | Http2.Frame.InternalError -> "INTERNAL_ERROR"
  | Http2.Frame.FlowControlError -> "FLOW_CONTROL_ERROR"
  | Http2.Frame.SettingsTimeout -> "SETTINGS_TIMEOUT"
  | Http2.Frame.StreamClosed -> "STREAM_CLOSED"
  | Http2.Frame.FrameSizeError -> "FRAME_SIZE_ERROR"
  | Http2.Frame.RefusedStream -> "REFUSED_STREAM"
  | Http2.Frame.Cancel -> "CANCEL"
  | Http2.Frame.CompressionError -> "COMPRESSION_ERROR"
  | Http2.Frame.ConnectError -> "CONNECT_ERROR"
  | Http2.Frame.EnhanceYourCalm -> "ENHANCE_YOUR_CALM"
  | Http2.Frame.InadequateSecurity -> "INADEQUATE_SECURITY"
  | Http2.Frame.Http11Required -> "HTTP_1_1_REQUIRED"
  | Http2.Frame.UnknownErrorCode code -> "UNKNOWN(" ^ Int.to_string code ^ ")"

let setting_json = fun __tmp1 ->
  match __tmp1 with
  | Http2.Frame.HeaderTableSize value ->
      Json.obj [ ("type", Json.string "HeaderTableSize"); ("value", Json.int value); ]
  | Http2.Frame.EnablePush value ->
      Json.obj [ ("type", Json.string "EnablePush"); ("value", Json.bool value); ]
  | Http2.Frame.MaxConcurrentStreams value ->
      Json.obj [ ("type", Json.string "MaxConcurrentStreams"); ("value", Json.int value); ]
  | Http2.Frame.InitialWindowSize value ->
      Json.obj [ ("type", Json.string "InitialWindowSize"); ("value", Json.int value); ]
  | Http2.Frame.MaxFrameSize value ->
      Json.obj [ ("type", Json.string "MaxFrameSize"); ("value", Json.int value); ]
  | Http2.Frame.MaxHeaderListSize value ->
      Json.obj [ ("type", Json.string "MaxHeaderListSize"); ("value", Json.int value); ]

let payload_json = fun __tmp1 ->
  match __tmp1 with
  | Http2.Frame.DataPayload { data; pad_length } ->
      let data =
        let len = String.length data in
        if len > 64 then
          "<" ^ Int.to_string len ^ " bytes>"
        else
          data
      in
      Json.obj
        [
          ("type", Json.string "DataPayload");
          ("data", Json.string data);
          ("pad_length", json_of_option Json.int pad_length);
        ]
  | Http2.Frame.HeadersPayload {
      pad_length;
      stream_dependency;
      weight;
      exclusive;
      header_block_fragment;
    } ->
      Json.obj
        [
          ("type", Json.string "HeadersPayload");
          ("pad_length", json_of_option Json.int pad_length);
          ("stream_dependency", json_of_option Json.int stream_dependency);
          ("weight", json_of_option Json.int weight);
          ("exclusive", Json.bool exclusive);
          ("header_block_fragment", Json.string (hex_string header_block_fragment));
        ]
  | Http2.Frame.PriorityPayload { stream_dependency; exclusive; weight } ->
      Json.obj
        [
          ("type", Json.string "PriorityPayload");
          ("stream_dependency", Json.int stream_dependency);
          ("exclusive", Json.bool exclusive);
          ("weight", Json.int weight);
        ]
  | Http2.Frame.RstStreamPayload error_code ->
      Json.obj
        [
          ("type", Json.string "RstStreamPayload");
          ("error_code", Json.string (error_code_string error_code));
        ]
  | Http2.Frame.SettingsPayload settings ->
      Json.obj
        [
          ("type", Json.string "SettingsPayload");
          ("settings", Json.array (List.map settings ~fn:setting_json));
        ]
  | Http2.Frame.PushPromisePayload { pad_length; promised_stream_id; header_block_fragment } ->
      Json.obj
        [
          ("type", Json.string "PushPromisePayload");
          ("pad_length", json_of_option Json.int pad_length);
          ("promised_stream_id", Json.int promised_stream_id);
          ("header_block_fragment", Json.string (hex_string header_block_fragment));
        ]
  | Http2.Frame.PingPayload data ->
      Json.obj [ ("type", Json.string "PingPayload"); ("data", Json.string (hex_string data)); ]
  | Http2.Frame.GoawayPayload { last_stream_id; error_code; debug_data } ->
      Json.obj
        [
          ("type", Json.string "GoawayPayload");
          ("last_stream_id", Json.int last_stream_id);
          ("error_code", Json.string (error_code_string error_code));
          ("debug_data", Json.string debug_data);
        ]
  | Http2.Frame.WindowUpdatePayload increment ->
      Json.obj [ ("type", Json.string "WindowUpdatePayload"); ("increment", Json.int increment); ]
  | Http2.Frame.ContinuationPayload data ->
      Json.obj
        [ ("type", Json.string "ContinuationPayload"); ("data", Json.string (hex_string data)); ]
  | Http2.Frame.UnknownPayload data ->
      Json.obj [ ("type", Json.string "UnknownPayload"); ("data", Json.string (hex_string data)); ]

let http2_frame_json = fun frame ->
  Json.obj
    [
      ("length", Json.int frame.Http2.Frame.length);
      ("frame_type", Json.string (frame_type_string frame.frame_type));
      ("flags", flags_json frame.flags);
      ("stream_id", Json.int frame.stream_id);
      ("payload", payload_json frame.payload);
    ]

let opcode_string = fun __tmp1 ->
  match __tmp1 with
  | Ws.Frame.Continuation -> "Continuation"
  | Ws.Frame.Text -> "Text"
  | Ws.Frame.Binary -> "Binary"
  | Ws.Frame.Close -> "Close"
  | Ws.Frame.Ping -> "Ping"
  | Ws.Frame.Pong -> "Pong"

let byte_at = fun value at ->
  value
  |> String.get_unchecked ~at
  |> Char.to_int

let websocket_is_masked = fun bytes -> String.length bytes >= 2 && byte_at bytes 1 land 0x80 != 0

let websocket_mask_key = fun bytes ->
  if not (websocket_is_masked bytes) then
    None
  else
    let payload_len = byte_at bytes 1 land 0x7f in
    let offset =
      if payload_len < 126 then
        2
      else if payload_len = 126 then
        4
      else
        10
    in
    if String.length bytes < offset + 4 then
      None
    else
      Some (
        String.sub bytes ~offset ~len:4
        |> hex_string
      )

let websocket_payload_json = fun frame ->
  match frame.Ws.Frame.opcode with
  | Ws.Frame.Binary -> Json.string (hex_string frame.payload)
  | Ws.Frame.Close ->
      if String.length frame.payload >= 2 then
        let status_code = (byte_at frame.payload 0 lsl 8) lor byte_at frame.payload 1 in
        let reason = String.sub frame.payload ~offset:2 ~len:(String.length frame.payload - 2) in
        Json.obj [ ("status_code", Json.int status_code); ("reason", Json.string reason); ]
      else
        Json.string frame.payload
  | Ws.Frame.Continuation
  | Ws.Frame.Text
  | Ws.Frame.Ping
  | Ws.Frame.Pong -> Json.string frame.payload

let websocket_frame_json = fun raw frame ->
  let fields = [
    ("fin", Json.bool frame.Ws.Frame.fin);
    ("rsv1", Json.bool frame.rsv1);
    ("rsv2", Json.bool frame.rsv2);
    ("rsv3", Json.bool frame.rsv3);
    ("opcode", Json.string (opcode_string frame.opcode));
    ("masked", Json.bool frame.masked);
  ]
  in
  let fields =
    match websocket_mask_key raw with
    | None -> fields
    | Some mask_key -> fields @ [ ("mask_key", Json.string mask_key); ]
  in
  Json.obj (fields @ [ ("payload", websocket_payload_json frame); ])

let authority_userinfo = fun uri ->
  Uri.authority uri
  |> Option.and_then
    ~fn:(fun authority ->
      Uri.Authority.from_string authority
      |> Result.to_option
      |> Option.and_then ~fn:Uri.Authority.userinfo)

let uri_json = fun uri ->
  let fields = [
    ("scheme", json_of_option Json.string (Uri.scheme uri));
    ("authority", json_of_option Json.string (Uri.authority uri));
    ("path", Json.string (Uri.path uri));
    ("query", json_of_option Json.string (Uri.query uri));
    ("fragment", json_of_option Json.string (Uri.fragment uri));
  ]
  in
  let fields =
    match Uri.authority uri with
    | None -> fields
    | Some _ ->
        let authority_fields = [
          ("host", json_of_option Json.string (Uri.host uri));
          ("port", json_of_option Json.int (Uri.port uri));
        ]
        in
        let authority_fields =
          match authority_userinfo uri with
          | None -> authority_fields
          | Some userinfo -> ("userinfo", Json.string userinfo) :: authority_fields
        in
        fields @ authority_fields
  in
  Json.obj fields

let fixture_json = fun relpath source ->
  let relpath = Path.to_string relpath in
  if String.starts_with ~prefix:"http1/request/" relpath then
    fixture_request_json source
  else if String.starts_with ~prefix:"http1/request_errors/" relpath then
    fixture_request_error_json source
  else if String.starts_with ~prefix:"http1/response/" relpath then
    fixture_response_json source
  else if String.starts_with ~prefix:"http1/response_errors/" relpath then
    fixture_response_error_json source
  else if String.starts_with ~prefix:"http2/frames/" relpath then
    match Http2.Parser.parse_frame source with
    | Http2.Parser.Done { value; remaining = "" } -> Ok (http2_frame_json value)
    | Http2.Parser.Done { remaining; _ } ->
        Error ("HTTP/2 frame fixture left " ^ Int.to_string (String.length remaining) ^ " bytes")
    | Http2.Parser.Need_more -> Error "incomplete HTTP/2 frame fixture"
    | Http2.Parser.Error error -> Error (Http2.Parser.error_to_string error)
  else if String.starts_with ~prefix:"websocket/frames/" relpath then
    let role =
      if websocket_is_masked source then
        Ws.Parser.Server
      else
        Ws.Parser.Client
    in
    match Ws.Parser.parse ~role source with
    | Ws.Parser.Done { value; remaining = "" } -> Ok (websocket_frame_json source value)
    | Ws.Parser.Done { remaining; _ } ->
        Error ("WebSocket frame fixture left " ^ Int.to_string (String.length remaining) ^ " bytes")
    | Ws.Parser.Need_more -> Error "incomplete WebSocket frame fixture"
    | Ws.Parser.Error error -> Error (Ws.Parser.error_to_string error)
  else if String.starts_with ~prefix:"uri/" relpath then
    match Uri.from_string source with
    | Ok uri -> Ok (uri_json uri)
    | Error error -> Error (Http1.Common.error_to_string (Http1.Common.InvalidRequestTarget error))
  else
    Error ("unsupported HTTP fixture: " ^ relpath)

let run_fixture = fun (ctx: Test.FixtureRunner.ctx) ->
  let* source = read_file ctx.fixture_path in
  let* expected = read_expected_json ctx.fixture_path in
  let* actual = fixture_json ctx.fixture_relpath source in
  Test.Snapshot.assert_inline_json ~ctx:ctx.test ~actual ~expected

let main ~args =
  let tests = Test.FixtureRunner.cases () ~dir:fixture_root ~filter:keep_fixture ~run:run_fixture in
  Test.Cli.main ~name:"http:fixtures" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
