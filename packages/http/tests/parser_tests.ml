open Http
open Std
open Std.Data

let read_file path =
  match Fs.read (Path.v path) with
  | Ok content -> content
  | Error _ -> failwith (format "Failed to read file: %s" path)

let parse_expected_request json =
  match Json.of_string json with
  | Ok (Json.Object fields) -> (
      let get_string key =
        Option.and_then (List.assoc_opt key fields) (function
          | Json.String s -> Some s
          | _ -> None)
      in
      let get_object key =
        Option.and_then (List.assoc_opt key fields) (function
          | Json.Object obj -> Some obj
          | _ -> None)
      in
      match
        ( get_string "method",
          get_string "path",
          get_string "version",
          get_object "headers",
          get_string "body" )
      with
      | Some method_, Some path, Some version, Some headers_obj, Some body ->
          let headers =
            List.filter_map
              (fun (k, v) ->
                match v with Json.String s -> Some (k, s) | _ -> None)
              headers_obj
          in
          Some (method_, path, version, headers, body)
      | _ -> None)
  | _ -> None

let parse_expected_response json =
  match Json.of_string json with
  | Ok (Json.Object fields) -> (
      let get_string key =
        Option.and_then (List.assoc_opt key fields) (function
          | Json.String s -> Some s
          | _ -> None)
      in
      let get_int key =
        Option.and_then (List.assoc_opt key fields) (function
          | Json.Int n -> Some n
          | _ -> None)
      in
      let get_object key =
        Option.and_then (List.assoc_opt key fields) (function
          | Json.Object obj -> Some obj
          | _ -> None)
      in
      match
        ( get_string "version",
          get_int "status_code",
          get_string "reason",
          get_object "headers",
          get_string "body" )
      with
      | Some version, Some status_code, Some reason, Some headers_obj, Some body
        ->
          let headers =
            List.filter_map
              (fun (k, v) ->
                match v with Json.String s -> Some (k, s) | _ -> None)
              headers_obj
          in
          Some (version, status_code, reason, headers, body)
      | _ -> None)
  | _ -> None

let parse_expected_uri json =
  match Json.of_string json with
  | Ok (Json.Object fields) ->
      let get_string key =
        Option.and_then (List.assoc_opt key fields) (function
          | Json.String s -> Some s
          | Json.Null -> Some ""
          | _ -> None)
      in
      let get_opt_string key =
        match List.assoc_opt key fields with
        | Some (Json.String s) -> Some (Some s)
        | Some Json.Null -> Some None
        | None -> Some None
        | _ -> None
      in
      let get_opt_int key =
        match List.assoc_opt key fields with
        | Some (Json.Int n) -> Some (Some n)
        | Some Json.Null -> Some None
        | None -> Some None
        | _ -> None
      in
      Some (fields, get_opt_string, get_opt_int)
  | _ -> None

let test_http1_request (name, http_file, expected_file) =
  Test.case (format "HTTP/1.1 Request: %s" name) (fun () ->
      let input = read_file http_file in
      let expected_json = read_file expected_file in

      match parse_expected_request expected_json with
      | None ->
          Error (format "Failed to parse expected JSON for %s" expected_file)
      | Some (exp_method, exp_path, exp_version, exp_headers, exp_body) -> (
          match Http1.Request.parse input with
          | Http1.Common.Done { value = request; _ } ->
              let actual_method =
                Std.Net.Http.Request.method_ request
                |> Std.Net.Http.Method.to_string
              in
              let actual_path =
                Std.Net.Http.Request.uri request |> Std.Net.Uri.to_string
              in
              let actual_version =
                Std.Net.Http.Request.version request
                |> Std.Net.Http.Version.to_string
              in
              let actual_headers = Std.Net.Http.Request.headers request in
              let actual_body =
                Std.Net.Http.Request.body request
                |> Option.unwrap_or ~default:""
              in

              if actual_method <> exp_method then
                Error
                  (format "Method mismatch: expected %s, got %s" exp_method
                     actual_method)
              else if actual_path <> exp_path then
                Error
                  (format "Path mismatch: expected %s, got %s" exp_path
                     actual_path)
              else if actual_version <> exp_version then
                Error
                  (format "Version mismatch: expected %s, got %s" exp_version
                     actual_version)
              else if actual_body <> exp_body then
                Error
                  (format "Body mismatch: expected '%s', got '%s'" exp_body
                     actual_body)
              else
                let header_errors =
                  List.filter_map
                    (fun (name, exp_value) ->
                      match Std.Net.Http.Header.get actual_headers name with
                      | Some actual_value ->
                          if actual_value <> exp_value then
                            Some
                              (format "Header %s: expected '%s', got '%s'" name
                                 exp_value actual_value)
                          else None
                      | None ->
                          Some
                            (format "Missing header: %s (expected '%s')" name
                               exp_value))
                    exp_headers
                in
                if List.length header_errors > 0 then
                  Error (String.concat "\n" header_errors)
                else Ok ()
          | Http1.Common.Need_more -> Error "Parser returned Need_more"
          | Http1.Common.Error e -> Error (format "Parse error: %s" e)))

let test_http1_response (name, http_file, expected_file) =
  Test.case (format "HTTP/1.1 Response: %s" name) (fun () ->
      let input = read_file http_file in
      let expected_json = read_file expected_file in

      match parse_expected_response expected_json with
      | None ->
          Error (format "Failed to parse expected JSON for %s" expected_file)
      | Some (exp_version, exp_status, exp_reason, exp_headers, exp_body) -> (
          match Http1.Response.parse input with
          | Http1.Common.Done { value = response; _ } ->
              let actual_version =
                Std.Net.Http.Response.version response
                |> Std.Net.Http.Version.to_string
              in
              let actual_status =
                Std.Net.Http.Response.status response
                |> Std.Net.Http.Status.to_int
              in
              let actual_reason =
                Std.Net.Http.Response.status response
                |> Std.Net.Http.Status.reason_phrase
              in
              let actual_headers = Std.Net.Http.Response.headers response in
              let actual_body =
                Std.Net.Http.Response.body response
                |> Option.unwrap_or ~default:""
              in

              if actual_version <> exp_version then
                Error
                  (format "Version mismatch: expected %s, got %s" exp_version
                     actual_version)
              else if actual_status <> exp_status then
                Error
                  (format "Status code mismatch: expected %d, got %d" exp_status
                     actual_status)
              else if actual_reason <> exp_reason then
                Error
                  (format "Reason mismatch: expected %s, got %s" exp_reason
                     actual_reason)
              else if actual_body <> exp_body then
                Error
                  (format "Body mismatch: expected '%s', got '%s'" exp_body
                     actual_body)
              else
                let header_errors =
                  List.filter_map
                    (fun (name, exp_value) ->
                      match Std.Net.Http.Header.get actual_headers name with
                      | Some actual_value ->
                          if actual_value <> exp_value then
                            Some
                              (format "Header %s: expected '%s', got '%s'" name
                                 exp_value actual_value)
                          else None
                      | None ->
                          Some
                            (format "Missing header: %s (expected '%s')" name
                               exp_value))
                    exp_headers
                in
                if List.length header_errors > 0 then
                  Error (String.concat "\n" header_errors)
                else Ok ()
          | Http1.Common.Need_more -> Error "Parser returned Need_more"
          | Http1.Common.Error e -> Error (format "Parse error: %s" e)))

let load_fixtures base_path suffix =
  let fixtures_path = Path.v base_path in
  match Fs.read_dir fixtures_path with
  | Error _ -> []
  | Ok iter ->
      let entries = Std.Iter.MutIterator.to_list iter in
      let fixtures =
        List.filter_map
          (fun path ->
            let name = Path.basename path in
            if String.ends_with ~suffix name then
              let base =
                String.sub name 0 (String.length name - String.length suffix)
              in
              let http_file = format "%s/%s" base_path name in
              let expected_file = format "%s/%s.expected" base_path base in
              match Fs.exists (Path.v expected_file) with
              | Ok true -> Some (base, http_file, expected_file)
              | _ -> None
            else None)
          entries
      in
      List.sort (fun (a, _, _) (b, _, _) -> String.compare a b) fixtures

let uri_error_to_string = function
  | Std.Net.Uri.InvalidScheme -> "InvalidScheme"
  | Std.Net.Uri.InvalidAuthority -> "InvalidAuthority"
  | Std.Net.Uri.InvalidPath -> "InvalidPath"
  | Std.Net.Uri.InvalidQuery -> "InvalidQuery"
  | Std.Net.Uri.InvalidFragment -> "InvalidFragment"

let string_to_hex s =
  let buf = Buffer.create (String.length s * 2) in
  String.iter (fun c -> Buffer.add_string buf (format "%02x" (Char.code c))) s;
  Buffer.contents buf

let parse_expected_http2_frame json =
  match Json.of_string json with
  | Ok (Json.Object fields) ->
      let get_int key =
        Option.and_then (List.assoc_opt key fields) (function
          | Json.Int n -> Some n
          | _ -> None)
      in
      let get_string key =
        Option.and_then (List.assoc_opt key fields) (function
          | Json.String s -> Some s
          | _ -> None)
      in
      let get_object key =
        Option.and_then (List.assoc_opt key fields) (function
          | Json.Object obj -> Some obj
          | _ -> None)
      in
      let get_bool key obj =
        Option.and_then (List.assoc_opt key obj) (function
          | Json.Bool b -> Some b
          | _ -> None)
      in
      let get_array key obj =
        Option.and_then (List.assoc_opt key obj) (function
          | Json.Array arr -> Some arr
          | _ -> None)
      in
      Some (get_int, get_string, get_object, get_bool, get_array)
  | _ -> None

let validate_http2_flags expected_obj actual_flags errors =
  let get_bool key obj =
    Option.and_then (List.assoc_opt key obj) (function
      | Json.Bool b -> Some b
      | _ -> None)
  in
  let check_flag name expected actual =
    if expected <> actual then
      errors :=
        format "flags.%s mismatch: expected %b, got %b" name expected actual
        :: !errors
  in
  Option.iter
    (fun v -> check_flag "end_stream" v actual_flags.Http2.Frame.end_stream)
    (get_bool "end_stream" expected_obj);
  Option.iter
    (fun v -> check_flag "end_headers" v actual_flags.end_headers)
    (get_bool "end_headers" expected_obj);
  Option.iter
    (fun v -> check_flag "padded" v actual_flags.padded)
    (get_bool "padded" expected_obj);
  Option.iter
    (fun v -> check_flag "priority" v actual_flags.priority)
    (get_bool "priority" expected_obj);
  Option.iter
    (fun v -> check_flag "ack" v actual_flags.ack)
    (get_bool "ack" expected_obj)

let error_code_to_string = function
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

let validate_http2_payload expected_obj actual_payload errors =
  match List.assoc_opt "type" expected_obj with
  | Some (Json.String "SettingsPayload") -> (
      match actual_payload with
      | Http2.Frame.SettingsPayload settings -> (
          match List.assoc_opt "settings" expected_obj with
          | Some (Json.Array exp_settings) ->
              if List.length exp_settings <> List.length settings then
                errors :=
                  format "payload.settings length mismatch: expected %d, got %d"
                    (List.length exp_settings) (List.length settings)
                  :: !errors
              else
                List.iter2
                  (fun exp_json actual_setting ->
                    match exp_json with
                    | Json.Object setting_obj -> (
                        match List.assoc_opt "type" setting_obj with
                        | Some (Json.String "HeaderTableSize") -> (
                            match actual_setting with
                            | Http2.Frame.HeaderTableSize actual_val -> (
                                match List.assoc_opt "value" setting_obj with
                                | Some (Json.Int exp_val) ->
                                    if exp_val <> actual_val then
                                      errors :=
                                        format
                                          "HeaderTableSize mismatch: expected \
                                           %d, got %d"
                                          exp_val actual_val
                                        :: !errors
                                | _ -> ())
                            | _ ->
                                errors :=
                                  "Setting type mismatch: expected \
                                   HeaderTableSize" :: !errors)
                        | Some (Json.String "EnablePush") -> (
                            match actual_setting with
                            | Http2.Frame.EnablePush actual_val -> (
                                match List.assoc_opt "value" setting_obj with
                                | Some (Json.Bool exp_val) ->
                                    if exp_val <> actual_val then
                                      errors :=
                                        format
                                          "EnablePush mismatch: expected %b, \
                                           got %b"
                                          exp_val actual_val
                                        :: !errors
                                | _ -> ())
                            | _ ->
                                errors :=
                                  "Setting type mismatch: expected EnablePush"
                                  :: !errors)
                        | Some (Json.String "MaxConcurrentStreams") -> (
                            match actual_setting with
                            | Http2.Frame.MaxConcurrentStreams actual_val -> (
                                match List.assoc_opt "value" setting_obj with
                                | Some (Json.Int exp_val) ->
                                    if exp_val <> actual_val then
                                      errors :=
                                        format
                                          "MaxConcurrentStreams mismatch: \
                                           expected %d, got %d"
                                          exp_val actual_val
                                        :: !errors
                                | _ -> ())
                            | _ ->
                                errors :=
                                  "Setting type mismatch: expected \
                                   MaxConcurrentStreams" :: !errors)
                        | Some (Json.String "InitialWindowSize") -> (
                            match actual_setting with
                            | Http2.Frame.InitialWindowSize actual_val -> (
                                match List.assoc_opt "value" setting_obj with
                                | Some (Json.Int exp_val) ->
                                    if exp_val <> actual_val then
                                      errors :=
                                        format
                                          "InitialWindowSize mismatch: \
                                           expected %d, got %d"
                                          exp_val actual_val
                                        :: !errors
                                | _ -> ())
                            | _ ->
                                errors :=
                                  "Setting type mismatch: expected \
                                   InitialWindowSize" :: !errors)
                        | Some (Json.String "MaxFrameSize") -> (
                            match actual_setting with
                            | Http2.Frame.MaxFrameSize actual_val -> (
                                match List.assoc_opt "value" setting_obj with
                                | Some (Json.Int exp_val) ->
                                    if exp_val <> actual_val then
                                      errors :=
                                        format
                                          "MaxFrameSize mismatch: expected %d, \
                                           got %d"
                                          exp_val actual_val
                                        :: !errors
                                | _ -> ())
                            | _ ->
                                errors :=
                                  "Setting type mismatch: expected MaxFrameSize"
                                  :: !errors)
                        | Some (Json.String "MaxHeaderListSize") -> (
                            match actual_setting with
                            | Http2.Frame.MaxHeaderListSize actual_val -> (
                                match List.assoc_opt "value" setting_obj with
                                | Some (Json.Int exp_val) ->
                                    if exp_val <> actual_val then
                                      errors :=
                                        format
                                          "MaxHeaderListSize mismatch: \
                                           expected %d, got %d"
                                          exp_val actual_val
                                        :: !errors
                                | _ -> ())
                            | _ ->
                                errors :=
                                  "Setting type mismatch: expected \
                                   MaxHeaderListSize" :: !errors)
                        | _ -> ())
                    | _ -> ())
                  exp_settings settings
          | _ -> ())
      | _ ->
          errors := "Payload type mismatch: expected SettingsPayload" :: !errors
      )
  | Some (Json.String "DataPayload") -> (
      match actual_payload with
      | Http2.Frame.DataPayload { data; pad_length } -> (
          (match List.assoc_opt "data" expected_obj with
          | Some (Json.String exp_data) ->
              let data_matches =
                if
                  String.starts_with ~prefix:"<" exp_data
                  && String.ends_with ~suffix:" bytes>" exp_data
                then
                  let len_str =
                    String.sub exp_data 1 (String.length exp_data - 8)
                  in
                  match int_of_string_opt len_str with
                  | Some expected_len -> String.length data = expected_len
                  | None -> false
                else exp_data = data
              in
              if not data_matches then
                errors :=
                  format "payload.data mismatch: expected '%s', got '%s'"
                    exp_data data
                  :: !errors
          | _ -> ());
          match List.assoc_opt "pad_length" expected_obj with
          | Some Json.Null ->
              if pad_length <> None then
                errors :=
                  format "payload.pad_length mismatch: expected None, got Some"
                  :: !errors
          | Some (Json.Int exp_pad) -> (
              match pad_length with
              | Some actual_pad ->
                  if exp_pad <> actual_pad then
                    errors :=
                      format "payload.pad_length mismatch: expected %d, got %d"
                        exp_pad actual_pad
                      :: !errors
              | None ->
                  errors :=
                    format
                      "payload.pad_length mismatch: expected Some %d, got None"
                      exp_pad
                    :: !errors)
          | _ -> ())
      | _ -> errors := "Payload type mismatch: expected DataPayload" :: !errors)
  | Some (Json.String "PingPayload") -> (
      match actual_payload with
      | Http2.Frame.PingPayload data -> (
          match List.assoc_opt "data" expected_obj with
          | Some (Json.String exp_data) ->
              let actual_hex = string_to_hex data in
              if exp_data <> actual_hex then
                errors :=
                  format "payload.data mismatch: expected '%s', got '%s'"
                    exp_data actual_hex
                  :: !errors
          | _ -> ())
      | _ -> errors := "Payload type mismatch: expected PingPayload" :: !errors)
  | Some (Json.String "WindowUpdatePayload") -> (
      match actual_payload with
      | Http2.Frame.WindowUpdatePayload increment -> (
          match List.assoc_opt "increment" expected_obj with
          | Some (Json.Int exp_inc) ->
              if exp_inc <> increment then
                errors :=
                  format "payload.increment mismatch: expected %d, got %d"
                    exp_inc increment
                  :: !errors
          | _ -> ())
      | _ ->
          errors :=
            "Payload type mismatch: expected WindowUpdatePayload" :: !errors)
  | Some (Json.String "GoawayPayload") -> (
      match actual_payload with
      | Http2.Frame.GoawayPayload { last_stream_id; error_code; debug_data }
        -> (
          (match List.assoc_opt "last_stream_id" expected_obj with
          | Some (Json.Int exp_id) ->
              if exp_id <> last_stream_id then
                errors :=
                  format "payload.last_stream_id mismatch: expected %d, got %d"
                    exp_id last_stream_id
                  :: !errors
          | _ -> ());
          (match List.assoc_opt "error_code" expected_obj with
          | Some (Json.String exp_code) ->
              let actual_code = error_code_to_string error_code in
              if exp_code <> actual_code then
                errors :=
                  format "payload.error_code mismatch: expected %s, got %s"
                    exp_code actual_code
                  :: !errors
          | _ -> ());
          match List.assoc_opt "debug_data" expected_obj with
          | Some (Json.String exp_debug) ->
              if exp_debug <> debug_data then
                errors :=
                  format "payload.debug_data mismatch: expected '%s', got '%s'"
                    exp_debug debug_data
                  :: !errors
          | _ -> ())
      | _ ->
          errors := "Payload type mismatch: expected GoawayPayload" :: !errors)
  | _ -> ()

let parse_expected_ws_frame json =
  match Json.of_string json with
  | Ok (Json.Object fields) ->
      let get_bool key =
        Option.and_then (List.assoc_opt key fields) (function
          | Json.Bool b -> Some b
          | _ -> None)
      in
      let get_string key =
        Option.and_then (List.assoc_opt key fields) (function
          | Json.String s -> Some s
          | _ -> None)
      in
      Some (get_bool, get_string)
  | _ -> None

let test_uri (name, uri_file, expected_file) =
  Test.case (format "URI: %s" name) (fun () ->
      let input = read_file uri_file in
      let expected_json = read_file expected_file in

      match parse_expected_uri expected_json with
      | None ->
          Error (format "Failed to parse expected JSON for %s" expected_file)
      | Some (fields, get_opt_string, get_opt_int) -> (
          match Std.Net.Uri.of_string input with
          | Error e ->
              Error (format "Failed to parse URI: %s" (uri_error_to_string e))
          | Ok uri ->
              let errors = ref [] in

              let check_opt_string field_name getter =
                match get_opt_string field_name with
                | Some expected ->
                    let actual = getter uri in
                    if actual <> expected then
                      errors :=
                        format "%s mismatch: expected %s, got %s" field_name
                          (Option.map_or ~default:"null" Fun.id expected)
                          (Option.map_or ~default:"null" Fun.id actual)
                        :: !errors
                | None -> ()
              in

              let check_opt_int field_name getter =
                match get_opt_int field_name with
                | Some expected ->
                    let actual = getter uri in
                    if actual <> expected then
                      errors :=
                        format "%s mismatch: expected %s, got %s" field_name
                          (Option.map_or ~default:"null" Int.to_string expected)
                          (Option.map_or ~default:"null" Int.to_string actual)
                        :: !errors
                | None -> ()
              in

              check_opt_string "scheme" Std.Net.Uri.scheme;
              check_opt_string "authority" Std.Net.Uri.authority;
              check_opt_string "host" Std.Net.Uri.host;
              check_opt_int "port" Std.Net.Uri.port;
              check_opt_string "query" Std.Net.Uri.query;
              check_opt_string "fragment" Std.Net.Uri.fragment;

              let exp_path = List.assoc_opt "path" fields in
              let actual_path = Std.Net.Uri.path uri in
              (match exp_path with
              | Some (Json.String exp) ->
                  if actual_path <> exp then
                    errors :=
                      format "path mismatch: expected '%s', got '%s'" exp
                        actual_path
                      :: !errors
              | _ -> ());

              if List.length !errors > 0 then Error (String.concat "\n" !errors)
              else Ok ()))

let frame_type_to_string = function
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

let test_http2_frame (name, frame_file, expected_file) =
  Test.case (format "HTTP/2 Frame: %s" name) (fun () ->
      let input = read_file frame_file in
      let expected_json = read_file expected_file in

      match parse_expected_http2_frame expected_json with
      | None ->
          Error (format "Failed to parse expected JSON for %s" expected_file)
      | Some (get_int, get_string, get_object, _get_bool, _get_array) -> (
          match Http2.Parser.parse_frame input with
          | Http2.Parser.Done { value = frame; _ } ->
              let errors = ref [] in

              (match get_int "length" with
              | Some exp_len ->
                  if frame.Http2.Frame.length <> exp_len then
                    errors :=
                      format "length mismatch: expected %d, got %d" exp_len
                        frame.length
                      :: !errors
              | None -> ());

              (match get_string "frame_type" with
              | Some exp_type ->
                  let actual_type = frame_type_to_string frame.frame_type in
                  if actual_type <> exp_type then
                    errors :=
                      format "frame_type mismatch: expected %s, got %s" exp_type
                        actual_type
                      :: !errors
              | None -> ());

              (match get_int "stream_id" with
              | Some exp_stream_id ->
                  if frame.stream_id <> exp_stream_id then
                    errors :=
                      format "stream_id mismatch: expected %d, got %d"
                        exp_stream_id frame.stream_id
                      :: !errors
              | None -> ());

              (match get_object "flags" with
              | Some flags_obj ->
                  validate_http2_flags flags_obj frame.flags errors
              | None -> ());

              (match get_object "payload" with
              | Some payload_obj ->
                  validate_http2_payload payload_obj frame.payload errors
              | None -> ());

              if List.length !errors > 0 then Error (String.concat "\n" !errors)
              else Ok ()
          | Http2.Parser.Need_more -> Error "Parser returned Need_more"
          | Http2.Parser.Error e -> Error (format "Parse error: %s" e)))

let opcode_to_string = function
  | Ws.Frame.Continuation -> "Continuation"
  | Ws.Frame.Text -> "Text"
  | Ws.Frame.Binary -> "Binary"
  | Ws.Frame.Close -> "Close"
  | Ws.Frame.Ping -> "Ping"
  | Ws.Frame.Pong -> "Pong"

let test_ws_frame (name, frame_file, expected_file) =
  Test.case (format "WebSocket Frame: %s" name) (fun () ->
      let input = read_file frame_file in
      let expected_json = read_file expected_file in

      match parse_expected_ws_frame expected_json with
      | None ->
          Error (format "Failed to parse expected JSON for %s" expected_file)
      | Some (get_bool, get_string) -> (
          match Ws.Parser.parse input with
          | Ws.Parser.Done { value = frame; _ } ->
              let errors = ref [] in

              (match get_bool "fin" with
              | Some exp_fin ->
                  if frame.Ws.Frame.fin <> exp_fin then
                    errors :=
                      format "fin mismatch: expected %b, got %b" exp_fin
                        frame.fin
                      :: !errors
              | None -> ());

              (match get_bool "masked" with
              | Some exp_masked ->
                  if frame.masked <> exp_masked then
                    errors :=
                      format "masked mismatch: expected %b, got %b" exp_masked
                        frame.masked
                      :: !errors
              | None -> ());

              (match get_string "opcode" with
              | Some exp_opcode ->
                  let actual_opcode = opcode_to_string frame.opcode in
                  if actual_opcode <> exp_opcode then
                    errors :=
                      format "opcode mismatch: expected %s, got %s" exp_opcode
                        actual_opcode
                      :: !errors
              | None -> ());

              (match get_string "payload" with
              | Some exp_payload ->
                  let actual_payload =
                    match frame.opcode with
                    | Ws.Frame.Binary -> string_to_hex frame.payload
                    | _ -> frame.payload
                  in
                  if actual_payload <> exp_payload then
                    errors :=
                      format "payload mismatch: expected '%s', got '%s'"
                        exp_payload actual_payload
                      :: !errors
              | None -> ());

              if List.length !errors > 0 then Error (String.concat "\n" !errors)
              else Ok ()
          | Ws.Parser.Need_more -> Error "Parser returned Need_more"
          | Ws.Parser.Error e -> Error (format "Parse error: %s" e)))

let () =
  Miniriot.run
    ~main:(fun ~args ->
      let request_fixtures =
        load_fixtures "packages/http/tests/fixtures/http1/request" ".http"
      in
      let response_fixtures =
        load_fixtures "packages/http/tests/fixtures/http1/response" ".http"
      in
      let uri_fixtures =
        load_fixtures "packages/http/tests/fixtures/uri" ".txt"
      in
      let http2_fixtures =
        load_fixtures "packages/http/tests/fixtures/http2/frames" ".frame"
      in
      let ws_fixtures =
        load_fixtures "packages/http/tests/fixtures/websocket/frames" ".frame"
      in

      let request_tests = List.map test_http1_request request_fixtures in
      let response_tests = List.map test_http1_response response_fixtures in
      let uri_tests = List.map test_uri uri_fixtures in
      let http2_tests = List.map test_http2_frame http2_fixtures in
      let ws_tests = List.map test_ws_frame ws_fixtures in

      let all_tests =
        request_tests @ response_tests @ uri_tests @ http2_tests @ ws_tests
      in

      Test.Cli.main ~name:"http" ~tests:all_tests ~args)
    ~args:Env.args ()
