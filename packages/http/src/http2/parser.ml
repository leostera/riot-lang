open Std

let ( let* ) = Result.and_then

type 'a parse_result =
  | Done of { value: 'a; remaining: string }
  | Need_more
  | Error of error

and error =
  | FailedToRead of read_field
  | FrameSizeExceedsMaximum of { size: int; max_size: int }
  | InvalidStreamId of {
      frame_type: Frame.frame_type;
      stream_id: int;
      expected: stream_id_rule;
    }
  | InvalidPaddingLength of { length: int; pad_length: int }
  | InvalidHeadersFrameLength of { length: int; offset: int; pad_length: int }
  | InvalidPushPromiseFrameLength of { length: int; offset: int; pad_length: int }
  | InvalidPayloadLength of {
      frame_type: Frame.frame_type;
      expected: payload_length_rule;
      actual: int;
    }
  | MalformedPriorityPayload
  | SettingsAckWithPayload of { length: int }
  | SettingsLengthNotMultipleOfSix of { length: int }
  | InvalidSettingValue of { setting: setting_id; value: int }
  | WindowUpdateIncrementZero

and read_field =
  | FrameLength
  | FrameType
  | Flags
  | StreamId
  | Priority
  | ErrorCode
  | SettingId
  | SettingValue
  | PromisedStreamId
  | LastStreamId
  | WindowSizeIncrement

and stream_id_rule =
  | MustBeZero
  | MustBeNonZero

and payload_length_rule =
  | Exactly of int
  | AtLeast of int
  | MultipleOf of int

and setting_id =
  | EnablePush
  | InitialWindowSize
  | MaxFrameSize

(* RFC 9113: SETTINGS_MAX_FRAME_SIZE default is 16384, max is 16777215 *)

(* For security, we enforce a configurable maximum *)

let default_max_frame_size = 16_384

(* 16 KB *)

let absolute_max_frame_size = 16_777_215

(* ~16 MB, RFC maximum *)

type config = { max_frame_size: int }

let default_config = { max_frame_size = default_max_frame_size }

let setting_id_to_string = function
  | EnablePush -> "SETTINGS_ENABLE_PUSH"
  | InitialWindowSize -> "SETTINGS_INITIAL_WINDOW_SIZE"
  | MaxFrameSize -> "SETTINGS_MAX_FRAME_SIZE"

let frame_type_name = function
  | Frame.Data -> "DATA"
  | Frame.Headers -> "HEADERS"
  | Frame.Priority -> "PRIORITY"
  | Frame.RstStream -> "RST_STREAM"
  | Frame.Settings -> "SETTINGS"
  | Frame.PushPromise -> "PUSH_PROMISE"
  | Frame.Ping -> "PING"
  | Frame.Goaway -> "GOAWAY"
  | Frame.WindowUpdate -> "WINDOW_UPDATE"
  | Frame.Continuation -> "CONTINUATION"
  | Frame.Unknown code -> "UNKNOWN(" ^ Int.to_string code ^ ")"

let read_field_to_string = function
  | FrameLength -> "frame length"
  | FrameType -> "frame type"
  | Flags -> "flags"
  | StreamId -> "stream ID"
  | Priority -> "priority fields"
  | ErrorCode -> "error code"
  | SettingId -> "setting ID"
  | SettingValue -> "setting value"
  | PromisedStreamId -> "promised stream ID"
  | LastStreamId -> "last stream ID"
  | WindowSizeIncrement -> "window size increment"

let payload_length_rule_to_string = function
  | Exactly size -> "exactly " ^ Int.to_string size ^ " bytes"
  | AtLeast size -> "at least " ^ Int.to_string size ^ " bytes"
  | MultipleOf size -> "a multiple of " ^ Int.to_string size ^ " bytes"

let stream_id_rule_to_string = function
  | MustBeZero -> "stream ID 0"
  | MustBeNonZero -> "a non-zero stream ID"

let error_to_string = function
  | FailedToRead field -> "Failed to read HTTP/2 " ^ read_field_to_string field
  | FrameSizeExceedsMaximum { size; max_size } ->
      "HTTP/2 frame size "
      ^ Int.to_string size
      ^ " exceeds maximum allowed "
      ^ Int.to_string max_size
  | InvalidStreamId { frame_type; stream_id; expected } ->
      frame_type_name frame_type
      ^ " frame used stream ID "
      ^ Int.to_string stream_id
      ^ ", expected "
      ^ stream_id_rule_to_string expected
  | InvalidPaddingLength { length; pad_length } ->
      "HTTP/2 padding length "
      ^ Int.to_string pad_length
      ^ " exceeds frame payload length "
      ^ Int.to_string length
  | InvalidHeadersFrameLength { length; offset; pad_length } ->
      "Invalid HEADERS frame length "
      ^ Int.to_string length
      ^ " for offset "
      ^ Int.to_string offset
      ^ " and padding "
      ^ Int.to_string pad_length
  | InvalidPushPromiseFrameLength { length; offset; pad_length } ->
      "Invalid PUSH_PROMISE frame length "
      ^ Int.to_string length
      ^ " for offset "
      ^ Int.to_string offset
      ^ " and padding "
      ^ Int.to_string pad_length
  | InvalidPayloadLength { frame_type; expected; actual } ->
      frame_type_name frame_type
      ^ " frame payload length must be "
      ^ payload_length_rule_to_string expected
      ^ ", got "
      ^ Int.to_string actual
  | MalformedPriorityPayload -> "Malformed PRIORITY payload"
  | SettingsAckWithPayload { length } ->
      "SETTINGS ACK must have zero length, got " ^ Int.to_string length
  | SettingsLengthNotMultipleOfSix { length } ->
      "SETTINGS frame length must be a multiple of 6, got " ^ Int.to_string length
  | InvalidSettingValue { setting; value } ->
      setting_id_to_string setting ^ " has invalid value " ^ Int.to_string value
  | WindowUpdateIncrementZero -> "WINDOW_UPDATE increment must be non-zero"

let byte_at = fun data offset ->
  data
  |> String.get_unchecked ~at:offset
  |> Char.to_int

let read_uint24_be = fun data offset ->
  if offset + 3 > String.length data then
    None
  else
    let b0 = byte_at data offset in
    let b1 = byte_at data (offset + 1) in
    let b2 = byte_at data (offset + 2) in
    Some ((b0 lsl 16) lor (b1 lsl 8) lor b2)

let read_uint32_be = fun data offset ->
  if offset + 4 > String.length data then
    None
  else
    let b0 = byte_at data offset in
    let b1 = byte_at data (offset + 1) in
    let b2 = byte_at data (offset + 2) in
    let b3 = byte_at data (offset + 3) in
    Some ((b0 lsl 24) lor (b1 lsl 16) lor (b2 lsl 8) lor b3)

let read_uint16_be = fun data offset ->
  if offset + 2 > String.length data then
    None
  else
    let b0 = byte_at data offset in
    let b1 = byte_at data (offset + 1) in
    Some ((b0 lsl 8) lor b1)

let read_uint8 = fun data offset ->
  if offset >= String.length data then
    None
  else
    Some (byte_at data offset)

let int_to_frame_type = function
  | 0x0 -> Frame.Data
  | 0x1 -> Frame.Headers
  | 0x2 -> Frame.Priority
  | 0x3 -> Frame.RstStream
  | 0x4 -> Frame.Settings
  | 0x5 -> Frame.PushPromise
  | 0x6 -> Frame.Ping
  | 0x7 -> Frame.Goaway
  | 0x8 -> Frame.WindowUpdate
  | 0x9 -> Frame.Continuation
  | code -> Frame.Unknown code

let parse_flags = fun frame_type flags_byte ->
  let end_headers = flags_byte land 0b0000_0100 != 0 in
  let padded = flags_byte land 0b0000_1000 != 0 in
  let priority = flags_byte land 0b0010_0000 != 0 in
  let bit_0_set = flags_byte land 0b0000_0001 != 0 in
  let (end_stream, ack) =
    match frame_type with
    | Frame.Settings
    | Frame.Ping -> (false, bit_0_set)
    | Frame.Data
    | Frame.Headers -> (bit_0_set, false)
    | _ -> (false, false)
  in
  {
    Frame.end_stream;
    end_headers;
    padded;
    priority;
    ack;
  }

let validate_stream_id = fun frame_type stream_id ->
  match frame_type with
  | Frame.Data
  | Frame.Headers
  | Frame.Priority
  | Frame.RstStream
  | Frame.PushPromise
  | Frame.Continuation when stream_id = 0 ->
      Result.Error (InvalidStreamId { frame_type; stream_id; expected = MustBeNonZero })
  | Frame.Settings
  | Frame.Ping
  | Frame.Goaway when stream_id != 0 ->
      Result.Error (InvalidStreamId { frame_type; stream_id; expected = MustBeZero })
  | _ -> Result.Ok ()

let parse_frame_header = fun ?(config = default_config) data ->
  if String.length data < 9 then
    Need_more
  else
    match read_uint24_be data 0 with
    | None -> Error (FailedToRead FrameLength)
    | Some length -> (
        (* Security fix: Validate frame size to prevent memory exhaustion DoS *)
        if length > config.max_frame_size then
          Error (FrameSizeExceedsMaximum { size = length; max_size = config.max_frame_size })
        else
          (
            match read_uint8 data 3 with
            | None -> Error (FailedToRead FrameType)
            | Some type_byte ->
                let frame_type = int_to_frame_type type_byte in
                (
                  match read_uint8 data 4 with
                  | None -> Error (FailedToRead Flags)
                  | Some flags_byte -> (
                      let flags = parse_flags frame_type flags_byte in
                      match read_uint32_be data 5 with
                      | None -> Error (FailedToRead StreamId)
                      | Some stream_id_raw ->
                          let stream_id = stream_id_raw land 0x7fff_ffff in
                          (
                            match validate_stream_id frame_type stream_id with
                            | Result.Error error -> Error error
                            | Result.Ok () ->
                                Done {
                                  value = (length, frame_type, flags, stream_id);
                                  remaining = String.sub
                                    data
                                    ~offset:9
                                    ~len:(String.length data - 9);
                                }
                          )
                    )
                )
          )
      )

let parse_data_payload = fun length flags data ->
  let has_padding = flags.Frame.padded in
  if has_padding then
    if length < 1 then
      Error (InvalidPayloadLength {
        frame_type = Frame.Data;
        expected = AtLeast 1;
        actual = length;
      })
    else
      match read_uint8 data 0 with
      | None -> Need_more
      | Some pad_length ->
          if length < 1 + pad_length then
            Error (InvalidPaddingLength { length; pad_length })
          else if String.length data < length then
            Need_more
          else
            let data_length = length - pad_length - 1 in
            let payload_data = String.sub data ~offset:1 ~len:data_length in
            let remaining = String.sub data ~offset:length ~len:(String.length data - length) in
            Done {
              value = Frame.DataPayload { data = payload_data; pad_length = Some pad_length };
              remaining;
            }
  else if String.length data < length then
    Need_more
  else
    let payload_data = String.sub data ~offset:0 ~len:length in
    let remaining = String.sub data ~offset:length ~len:(String.length data - length) in
    Done { value = Frame.DataPayload { data = payload_data; pad_length = None }; remaining }

let parse_priority_fields = fun data offset ->
  match read_uint32_be data offset with
  | None -> None
  | Some dep_raw -> (
      let exclusive = dep_raw land 0x8000_0000 != 0 in
      let stream_dependency = dep_raw land 0x7fff_ffff in
      match read_uint8 data (offset + 4) with
      | None -> None
      | Some weight -> Some (stream_dependency, exclusive, weight + 1)
    )

let parse_headers_payload = fun length flags data ->
  let has_padding = flags.Frame.padded in
  let has_priority = flags.Frame.priority in
  let min_length =
    (
      if has_padding then
        1
      else
        0
    ) + (
      if has_priority then
        5
      else
        0
    )
  in
  if length < min_length then
    Error (InvalidPayloadLength {
      frame_type = Frame.Headers;
      expected = AtLeast min_length;
      actual = length;
    })
  else if String.length data < min_length then
    Need_more
  else
    let (pad_length_opt, offset) =
      if has_padding then
        match read_uint8 data 0 with
        | None -> (None, 0)
        | Some pl -> (Some pl, 1)
      else
        (None, 0)
    in
    let (priority_info, offset) =
      if has_priority then
        match parse_priority_fields data offset with
        | None -> (None, offset)
        | Some (dep, excl, w) -> (Some (dep, excl, w), offset + 5)
      else
        (None, offset)
    in
    let pad_length =
      match pad_length_opt with
      | Some p -> p
      | None -> 0
    in
    let header_fragment_length = length - offset - pad_length in
    if header_fragment_length < 0 then
      Error (InvalidHeadersFrameLength { length; offset; pad_length })
    else if String.length data < length then
      Need_more
    else
      let header_block_fragment = String.sub data ~offset ~len:header_fragment_length in
      let remaining = String.sub data ~offset:length ~len:(String.length data - length) in
      let (stream_dependency, weight, exclusive) =
        match priority_info with
        | Some (dep, excl, w) -> (Some dep, Some w, excl)
        | None -> (None, None, false)
      in
      Done {
        value =
          Frame.HeadersPayload {
            pad_length = pad_length_opt;
            stream_dependency;
            weight;
            exclusive;
            header_block_fragment;
          };
        remaining;
      }

let parse_priority_payload = fun length data ->
  if length != 5 then
    Error (InvalidPayloadLength {
      frame_type = Frame.Priority;
      expected = Exactly 5;
      actual = length;
    })
  else if String.length data < 5 then
    Need_more
  else
    match parse_priority_fields data 0 with
    | None -> Error MalformedPriorityPayload
    | Some (stream_dependency, exclusive, weight) ->
        let remaining = String.sub data ~offset:5 ~len:(String.length data - 5) in
        Done { value = Frame.PriorityPayload { stream_dependency; exclusive; weight }; remaining }

let parse_rst_stream_payload = fun length data ->
  if length != 4 then
    Error (InvalidPayloadLength {
      frame_type = Frame.RstStream;
      expected = Exactly 4;
      actual = length;
    })
  else if String.length data < 4 then
    Need_more
  else
    match read_uint32_be data 0 with
    | None -> Error (FailedToRead ErrorCode)
    | Some error_code_int ->
        let error_code =
          match Frame.int_to_error_code error_code_int with
          | Some ec -> ec
          | None -> Frame.ProtocolError
        in
        let remaining = String.sub data ~offset:4 ~len:(String.length data - 4) in
        Done { value = Frame.RstStreamPayload error_code; remaining }

let parse_settings_payload = fun length flags data ->
  if flags.Frame.ack && length != 0 then
    Error (SettingsAckWithPayload { length })
  else if length mod 6 != 0 then
    Error (SettingsLengthNotMultipleOfSix { length })
  else if String.length data < length then
    Need_more
  else
    let rec parse_settings offset acc =
      if offset >= length then
        Ok (List.reverse acc)
      else
        match read_uint16_be data offset with
        | None -> Result.Error (FailedToRead SettingId)
        | Some id -> (
            match read_uint32_be data (offset + 2) with
            | None -> Result.Error (FailedToRead SettingValue)
            | Some value -> (
                let setting =
                  match id with
                  | 0x1 -> Some (Frame.HeaderTableSize value)
                  | 0x2 ->
                      if value = 0 || value = 1 then
                        Some (Frame.EnablePush (value = 1))
                      else
                        None
                  | 0x3 -> Some (Frame.MaxConcurrentStreams value)
                  | 0x4 ->
                      if value <= 2_147_483_647 then
                        Some (Frame.InitialWindowSize value)
                      else
                        None
                  | 0x5 ->
                      if value >= default_max_frame_size && value <= absolute_max_frame_size then
                        Some (Frame.MaxFrameSize value)
                      else
                        None
                  | 0x6 -> Some (Frame.MaxHeaderListSize value)
                  | _ -> None
                in
                match setting with
                | Some s -> parse_settings (offset + 6) (s :: acc)
                | None -> (
                    match id with
                    | 0x2 -> Result.Error (InvalidSettingValue { setting = EnablePush; value })
                    | 0x4 ->
                        Result.Error (InvalidSettingValue { setting = InitialWindowSize; value })
                    | 0x5 -> Result.Error (InvalidSettingValue { setting = MaxFrameSize; value })
                    | _ -> parse_settings (offset + 6) acc
                  )
              )
          )
    in
    match parse_settings 0 [] with
    | Ok settings ->
        let remaining = String.sub data ~offset:length ~len:(String.length data - length) in
        Done { value = Frame.SettingsPayload settings; remaining }
    | Result.Error error -> Error error

let parse_push_promise_payload = fun length flags data ->
  let has_padding = flags.Frame.padded in
  let min_length =
    4 + if has_padding then
      1
    else
      0
  in
  if length < min_length then
    Error (InvalidPayloadLength {
      frame_type = Frame.PushPromise;
      expected = AtLeast min_length;
      actual = length;
    })
  else if String.length data < min_length then
    Need_more
  else
    let (pad_length_opt, offset) =
      if has_padding then
        match read_uint8 data 0 with
        | None -> (None, 0)
        | Some pl -> (Some pl, 1)
      else
        (None, 0)
    in
    if String.length data < offset + 4 then
      Need_more
    else
      match read_uint32_be data offset with
      | None -> Error (FailedToRead PromisedStreamId)
      | Some promised_stream_id_raw ->
          let promised_stream_id = promised_stream_id_raw land 0x7fff_ffff in
          let offset = offset + 4 in
          let pad_length =
            match pad_length_opt with
            | Some p -> p
            | None -> 0
          in
          let header_fragment_length = length - offset - pad_length in
          if header_fragment_length < 0 then
            Error (InvalidPushPromiseFrameLength { length; offset; pad_length })
          else if String.length data < length then
            Need_more
          else
            let header_block_fragment = String.sub data ~offset ~len:header_fragment_length in
            let remaining = String.sub data ~offset:length ~len:(String.length data - length) in
            Done {
              value = Frame.PushPromisePayload {
                pad_length = pad_length_opt;
                promised_stream_id;
                header_block_fragment;
              };
              remaining;
            }

let parse_ping_payload = fun length data ->
  if length != 8 then
    Error (InvalidPayloadLength { frame_type = Frame.Ping; expected = Exactly 8; actual = length })
  else if String.length data < 8 then
    Need_more
  else
    let opaque_data = String.sub data ~offset:0 ~len:8 in
    let remaining = String.sub data ~offset:8 ~len:(String.length data - 8) in
    Done { value = Frame.PingPayload opaque_data; remaining }

let parse_goaway_payload = fun length data ->
  if length < 8 then
    Error (InvalidPayloadLength {
      frame_type = Frame.Goaway;
      expected = AtLeast 8;
      actual = length;
    })
  else if String.length data < length then
    Need_more
  else
    match read_uint32_be data 0 with
    | None -> Error (FailedToRead LastStreamId)
    | Some last_stream_id_raw -> (
        let last_stream_id = last_stream_id_raw land 0x7fff_ffff in
        match read_uint32_be data 4 with
        | None -> Error (FailedToRead ErrorCode)
        | Some error_code_int ->
            let error_code =
              match Frame.int_to_error_code error_code_int with
              | Some ec -> ec
              | None -> Frame.ProtocolError
            in
            let debug_data = String.sub data ~offset:8 ~len:(length - 8) in
            let remaining = String.sub data ~offset:length ~len:(String.length data - length) in
            Done {
              value = Frame.GoawayPayload { last_stream_id; error_code; debug_data };
              remaining;
            }
      )

let parse_window_update_payload = fun length data ->
  if length != 4 then
    Error (InvalidPayloadLength {
      frame_type = Frame.WindowUpdate;
      expected = Exactly 4;
      actual = length;
    })
  else if String.length data < 4 then
    Need_more
  else
    match read_uint32_be data 0 with
    | None -> Error (FailedToRead WindowSizeIncrement)
    | Some increment_raw ->
        let increment = increment_raw land 0x7fff_ffff in
        if increment = 0 then
          Error WindowUpdateIncrementZero
        else
          let remaining = String.sub data ~offset:4 ~len:(String.length data - 4) in
          Done { value = Frame.WindowUpdatePayload increment; remaining }

let parse_continuation_payload = fun length data ->
  if String.length data < length then
    Need_more
  else
    let header_block_fragment = String.sub data ~offset:0 ~len:length in
    let remaining = String.sub data ~offset:length ~len:(String.length data - length) in
    Done { value = Frame.ContinuationPayload header_block_fragment; remaining }

let parse_payload = fun length frame_type flags data ->
  match frame_type with
  | Frame.Data -> parse_data_payload length flags data
  | Frame.Headers -> parse_headers_payload length flags data
  | Frame.Priority -> parse_priority_payload length data
  | Frame.RstStream -> parse_rst_stream_payload length data
  | Frame.Settings -> parse_settings_payload length flags data
  | Frame.PushPromise -> parse_push_promise_payload length flags data
  | Frame.Ping -> parse_ping_payload length data
  | Frame.Goaway -> parse_goaway_payload length data
  | Frame.WindowUpdate -> parse_window_update_payload length data
  | Frame.Continuation -> parse_continuation_payload length data
  | Frame.Unknown _ ->
      if String.length data < length then
        Need_more
      else
        Done {
          value = Frame.UnknownPayload (String.sub data ~offset:0 ~len:length);
          remaining = String.sub data ~offset:length ~len:(String.length data - length);
        }

let parse_frame = fun data ->
  match parse_frame_header data with
  | Error msg -> Error msg
  | Need_more -> Need_more
  | Done { value = (length, frame_type, flags, stream_id); remaining } -> (
      match parse_payload length frame_type flags remaining with
      | Error msg -> Error msg
      | Need_more -> Need_more
      | Done { value = payload; remaining } ->
          Done {
            value =
              {
                Frame.length;
                frame_type;
                flags;
                stream_id;
                payload;
              };
            remaining;
          }
    )
