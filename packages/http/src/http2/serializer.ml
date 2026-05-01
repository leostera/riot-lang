open Std

type payload_error = {
  frame_type: Frame.frame_type;
  payload: Frame.payload;
}

type stream_id_rule =
  | MustBeZero
  | MustBeNonZero

type setting_id =
  | HeaderTableSize
  | MaxConcurrentStreams
  | InitialWindowSize
  | MaxFrameSize
  | MaxHeaderListSize

type setting_value_rule =
  | Unsigned32
  | InitialWindowSizeRange
  | MaxFrameSizeRange

type error =
  | PayloadMismatch of payload_error
  | SettingsAckWithPayload of { setting_count: int }
  | InvalidPingPayloadLength of { length: int }
  | InvalidWindowUpdateIncrement of { increment: int }
  | PayloadLengthTooLarge of { length: int; max_length: int }
  | InvalidUnknownFrameTypeCode of { code: int }
  | InvalidStreamId of {
      frame_type: Frame.frame_type;
      stream_id: int;
      expected: stream_id_rule;
    }
  | InvalidPaddingLength of {
      frame_type: Frame.frame_type;
      pad_length: int;
    }
  | MissingPriorityFields of {
      frame_type: Frame.frame_type;
    }
  | InvalidPriorityWeight of { weight: int }
  | InvalidStreamDependency of { stream_dependency: int }
  | InvalidPriorityDependency of { stream_id: int; stream_dependency: int }
  | InvalidStreamIdRange of { stream_id: int }
  | InvalidPromisedStreamId of { promised_stream_id: int }
  | InvalidLastStreamId of { last_stream_id: int }
  | InvalidSettingValue of {
      setting: setting_id;
      value: int;
      expected: setting_value_rule;
    }
  | InvalidErrorCode of { code: int }

let frame_type_to_string = fun __tmp1 ->
  match __tmp1 with
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

let setting_id_to_string = fun __tmp1 ->
  match __tmp1 with
  | HeaderTableSize -> "SETTINGS_HEADER_TABLE_SIZE"
  | MaxConcurrentStreams -> "SETTINGS_MAX_CONCURRENT_STREAMS"
  | InitialWindowSize -> "SETTINGS_INITIAL_WINDOW_SIZE"
  | MaxFrameSize -> "SETTINGS_MAX_FRAME_SIZE"
  | MaxHeaderListSize -> "SETTINGS_MAX_HEADER_LIST_SIZE"

let setting_value_rule_to_string = fun __tmp1 ->
  match __tmp1 with
  | Unsigned32 -> "0..2^32-1"
  | InitialWindowSizeRange -> "0..2^31-1"
  | MaxFrameSizeRange -> "16384..16777215"

let error_to_string = fun __tmp1 ->
  match __tmp1 with
  | PayloadMismatch { frame_type; _ } ->
      "Payload does not match HTTP/2 " ^ frame_type_to_string frame_type ^ " frame"
  | SettingsAckWithPayload { setting_count } ->
      "SETTINGS ACK must not carry settings, got " ^ Int.to_string setting_count ^ " setting(s)"
  | InvalidPingPayloadLength { length } ->
      "PING opaque data must be exactly 8 bytes, got " ^ Int.to_string length
  | InvalidWindowUpdateIncrement { increment } ->
      "WINDOW_UPDATE increment must be between 1 and 2^31-1, got " ^ Int.to_string increment
  | PayloadLengthTooLarge { length; max_length } ->
      "HTTP/2 frame payload length "
      ^ Int.to_string length
      ^ " exceeds the 24-bit frame length limit "
      ^ Int.to_string max_length
  | InvalidUnknownFrameTypeCode { code } ->
      "HTTP/2 unknown frame type code must fit in one byte, got " ^ Int.to_string code
  | InvalidStreamId { frame_type; stream_id; expected } ->
      let expected =
        match expected with
        | MustBeZero -> "stream ID 0"
        | MustBeNonZero -> "a non-zero stream ID"
      in
      frame_type_to_string frame_type
      ^ " frame used stream ID "
      ^ Int.to_string stream_id
      ^ ", expected "
      ^ expected
  | InvalidPaddingLength { frame_type; pad_length } ->
      frame_type_to_string frame_type
      ^ " frame padding length must fit in one byte, got "
      ^ Int.to_string pad_length
  | MissingPriorityFields { frame_type } ->
      frame_type_to_string frame_type ^ " frame has incomplete priority fields"
  | InvalidPriorityWeight { weight } ->
      "HTTP/2 priority weight must be between 1 and 256, got " ^ Int.to_string weight
  | InvalidStreamDependency { stream_dependency } ->
      "HTTP/2 stream dependency must be between 0 and 2^31-1, got "
      ^ Int.to_string stream_dependency
  | InvalidPriorityDependency { stream_id; stream_dependency } ->
      "HTTP/2 stream "
      ^ Int.to_string stream_id
      ^ " cannot depend on itself as priority dependency "
      ^ Int.to_string stream_dependency
  | InvalidStreamIdRange { stream_id } ->
      "HTTP/2 stream ID must be between 0 and 2^31-1, got " ^ Int.to_string stream_id
  | InvalidPromisedStreamId { promised_stream_id } ->
      "HTTP/2 promised stream ID must be between 1 and 2^31-1, got "
      ^ Int.to_string promised_stream_id
  | InvalidLastStreamId { last_stream_id } ->
      "HTTP/2 last stream ID must be between 0 and 2^31-1, got " ^ Int.to_string last_stream_id
  | InvalidSettingValue { setting; value; expected } ->
      setting_id_to_string setting
      ^ " must be in range "
      ^ setting_value_rule_to_string expected
      ^ ", got "
      ^ Int.to_string value
  | InvalidErrorCode { code } ->
      "HTTP/2 error code must be between 0 and 2^32-1, got " ^ Int.to_string code

let write_uint24_be = fun value ->
  let b0 = Char.from_int_unchecked ((value lsr 16) land 0b1111_1111) in
  let b1 = Char.from_int_unchecked ((value lsr 8) land 0b1111_1111) in
  let b2 = Char.from_int_unchecked (value land 0b1111_1111) in
  String.make ~len:1 ~char:b0 ^ String.make ~len:1 ~char:b1 ^ String.make ~len:1 ~char:b2

let write_uint32_be = fun value ->
  let b0 = Char.from_int_unchecked ((value lsr 24) land 0b1111_1111) in
  let b1 = Char.from_int_unchecked ((value lsr 16) land 0b1111_1111) in
  let b2 = Char.from_int_unchecked ((value lsr 8) land 0b1111_1111) in
  let b3 = Char.from_int_unchecked (value land 0b1111_1111) in
  String.make ~len:1 ~char:b0
  ^ String.make ~len:1 ~char:b1
  ^ String.make ~len:1 ~char:b2
  ^ String.make ~len:1 ~char:b3

let write_uint16_be = fun value ->
  let b0 = Char.from_int_unchecked ((value lsr 8) land 0b1111_1111) in
  let b1 = Char.from_int_unchecked (value land 0b1111_1111) in
  String.make ~len:1 ~char:b0 ^ String.make ~len:1 ~char:b1

let write_uint8 = fun value ->
  String.make
    ~len:1
    ~char:(Char.from_int_unchecked (value land 0b1111_1111))

let frame_type_to_int = fun __tmp1 ->
  match __tmp1 with
  | Frame.Data -> Ok 0x0
  | Frame.Headers -> Ok 0x1
  | Frame.Priority -> Ok 0x2
  | Frame.RstStream -> Ok 0x3
  | Frame.Settings -> Ok 0x4
  | Frame.PushPromise -> Ok 0x5
  | Frame.Ping -> Ok 0x6
  | Frame.Goaway -> Ok 0x7
  | Frame.WindowUpdate -> Ok 0x8
  | Frame.Continuation -> Ok 0x9
  | Frame.Unknown code ->
      if code >= 0 && code <= 0b1111_1111 then
        Ok code
      else
        Error (InvalidUnknownFrameTypeCode { code })

let validate_stream_id = fun frame_type stream_id ->
  if stream_id < 0 || stream_id > 0x7fff_ffff then
    Error (InvalidStreamIdRange { stream_id })
  else
    match frame_type with
    | Frame.Data
    | Frame.Headers
    | Frame.Priority
    | Frame.RstStream
    | Frame.PushPromise
    | Frame.Continuation when stream_id = 0 ->
        Error (InvalidStreamId { frame_type; stream_id; expected = MustBeNonZero })
    | Frame.Settings
    | Frame.Ping
    | Frame.Goaway when stream_id != 0 ->
        Error (InvalidStreamId { frame_type; stream_id; expected = MustBeZero })
    | _ -> Ok ()

let validate_padding = fun frame_type pad_length ->
  match pad_length with
  | None -> Ok ()
  | Some pad_length ->
      if pad_length >= 0 && pad_length <= 0b1111_1111 then
        Ok ()
      else
        Error (InvalidPaddingLength { frame_type; pad_length })

let validate_stream_dependency = fun stream_dependency ->
  if stream_dependency >= 0 && stream_dependency <= 0x7fff_ffff then
    Ok ()
  else
    Error (InvalidStreamDependency { stream_dependency })

let validate_priority_weight = fun weight ->
  if weight >= 1 && weight <= 256 then
    Ok ()
  else
    Error (InvalidPriorityWeight { weight })

let validate_priority_dependency = fun ~stream_id ~stream_dependency ->
  if stream_dependency = stream_id then
    Error (InvalidPriorityDependency { stream_id; stream_dependency })
  else
    Ok ()

let validate_promised_stream_id = fun promised_stream_id ->
  if promised_stream_id >= 1 && promised_stream_id <= 0x7fff_ffff then
    Ok ()
  else
    Error (InvalidPromisedStreamId { promised_stream_id })

let validate_last_stream_id = fun last_stream_id ->
  if last_stream_id >= 0 && last_stream_id <= 0x7fff_ffff then
    Ok ()
  else
    Error (InvalidLastStreamId { last_stream_id })

let validate_error_code = fun error_code ->
  let code = Frame.error_code_to_int error_code in
  if code >= 0 && code <= 0xffff_ffff then
    Ok code
  else
    Error (InvalidErrorCode { code })

let validate_uint32_setting = fun setting value ->
  if value >= 0 && value <= 0xffff_ffff then
    Ok ()
  else
    Error (InvalidSettingValue { setting; value; expected = Unsigned32 })

let validate_setting_value = fun __tmp1 ->
  match __tmp1 with
  | Frame.HeaderTableSize value -> validate_uint32_setting HeaderTableSize value
  | Frame.EnablePush _ -> Ok ()
  | Frame.MaxConcurrentStreams value -> validate_uint32_setting MaxConcurrentStreams value
  | Frame.InitialWindowSize value ->
      if value >= 0 && value <= 0x7fff_ffff then
        Ok ()
      else
        Error (InvalidSettingValue {
          setting = InitialWindowSize;
          value;
          expected = InitialWindowSizeRange;
        })
  | Frame.MaxFrameSize value ->
      if value >= 16_384 && value <= 16_777_215 then
        Ok ()
      else
        Error (InvalidSettingValue { setting = MaxFrameSize; value; expected = MaxFrameSizeRange })
  | Frame.MaxHeaderListSize value -> validate_uint32_setting MaxHeaderListSize value

let flags_to_byte = fun frame_type flags ->
  let open Frame in
  let byte = 0 in
  let byte =
    if flags.end_stream then
      byte lor 0b0000_0001
    else
      byte
  in
  let byte =
    if flags.end_headers then
      byte lor 0b0000_0100
    else
      byte
  in
  let byte =
    if flags.padded then
      byte lor 0b0000_1000
    else
      byte
  in
  let byte =
    if flags.priority then
      byte lor 0b0010_0000
    else
      byte
  in
  let byte =
    if flags.ack then
      byte lor 0b0000_0001
    else
      byte
  in
  byte

let serialize_priority = fun ~stream_id stream_dependency exclusive weight ->
  match validate_stream_dependency stream_dependency with
  | Error error -> Error error
  | Ok () -> (
      match validate_priority_dependency ~stream_id ~stream_dependency with
      | Error error -> Error error
      | Ok () -> (
          match validate_priority_weight weight with
          | Error error -> Error error
          | Ok () ->
              let dep_with_exclusive =
                if exclusive then
                  stream_dependency lor 0x8000_0000
                else
                  stream_dependency
              in
              Ok (write_uint32_be dep_with_exclusive ^ write_uint8 (weight - 1))
        )
    )

let serialize_data_payload = fun payload ->
  match payload with
  | Frame.DataPayload { data; pad_length = None } -> (
      match validate_padding Frame.Data None with
      | Error error -> Error error
      | Ok () -> Ok data
    )
  | Frame.DataPayload { data; pad_length = Some pad_len } -> (
      match validate_padding Frame.Data (Some pad_len) with
      | Error error -> Error error
      | Ok () -> Ok (write_uint8 pad_len ^ data ^ String.make ~len:pad_len ~char:'\x00')
    )
  | payload -> Error (PayloadMismatch { frame_type = Frame.Data; payload })

let serialize_headers_payload = fun ~stream_id payload ->
  match payload with
  | Frame.HeadersPayload {
      pad_length;
      stream_dependency;
      weight;
      exclusive;
      header_block_fragment;
    } ->
      (
          match validate_padding Frame.Headers pad_length with
          | Error error -> Error error
          | Ok () -> (
              let pad_bytes =
                match pad_length with
                | Some pl -> write_uint8 pl
                | None -> ""
              in
              let priority_bytes =
                match (stream_dependency, weight) with
                | (Some dep, Some w) -> serialize_priority ~stream_id dep exclusive w
                | (None, None) -> Ok ""
                | _ -> Error (MissingPriorityFields { frame_type = Frame.Headers })
              in
              match priority_bytes with
              | Error error -> Error error
              | Ok priority_bytes ->
                  let padding =
                    match pad_length with
                    | Some pl -> String.make ~len:pl ~char:'\x00'
                    | None -> ""
                  in
                  Ok (pad_bytes ^ priority_bytes ^ header_block_fragment ^ padding)
            )
        )
  | payload -> Error (PayloadMismatch { frame_type = Frame.Headers; payload })

let serialize_priority_payload = fun ~stream_id payload ->
  match payload with
  | Frame.PriorityPayload { stream_dependency; exclusive; weight } ->
      serialize_priority ~stream_id stream_dependency exclusive weight
  | payload -> Error (PayloadMismatch { frame_type = Frame.Priority; payload })

let serialize_rst_stream_payload = fun payload ->
  match payload with
  | Frame.RstStreamPayload error_code -> (
      match validate_error_code error_code with
      | Error error -> Error error
      | Ok code -> Ok (write_uint32_be code)
    )
  | payload -> Error (PayloadMismatch { frame_type = Frame.RstStream; payload })

let serialize_setting = fun __tmp1 ->
  match __tmp1 with
  | Frame.HeaderTableSize value -> Ok (write_uint16_be 0x1 ^ write_uint32_be value)
  | Frame.EnablePush enabled ->
      Ok (
        write_uint16_be 0x2 ^ write_uint32_be
          (
            if enabled then
              1
            else
              0
          )
      )
  | Frame.MaxConcurrentStreams value -> Ok (write_uint16_be 0x3 ^ write_uint32_be value)
  | Frame.InitialWindowSize value -> Ok (write_uint16_be 0x4 ^ write_uint32_be value)
  | Frame.MaxFrameSize value -> Ok (write_uint16_be 0x5 ^ write_uint32_be value)
  | Frame.MaxHeaderListSize value -> Ok (write_uint16_be 0x6 ^ write_uint32_be value)

let serialize_settings_payload = fun payload ->
  match payload with
  | Frame.SettingsPayload settings -> (
      let rec loop acc = fun __tmp1 ->
        match __tmp1 with
        | [] -> Ok (String.concat "" (List.reverse acc))
        | setting :: rest -> (
            match validate_setting_value setting with
            | Error error -> Error error
            | Ok () -> (
                match serialize_setting setting with
                | Error error -> Error error
                | Ok bytes -> loop (bytes :: acc) rest
              )
          )
      in
      loop [] settings
    )
  | payload -> Error (PayloadMismatch { frame_type = Frame.Settings; payload })

let serialize_push_promise_payload = fun payload ->
  match payload with
  | Frame.PushPromisePayload { pad_length; promised_stream_id; header_block_fragment } -> (
      match validate_padding Frame.PushPromise pad_length with
      | Error error -> Error error
      | Ok () -> (
          match validate_promised_stream_id promised_stream_id with
          | Error error -> Error error
          | Ok () ->
              let pad_bytes =
                match pad_length with
                | Some pl -> write_uint8 pl
                | None -> ""
              in
              let promised_id_bytes = write_uint32_be promised_stream_id in
              let padding =
                match pad_length with
                | Some pl -> String.make ~len:pl ~char:'\x00'
                | None -> ""
              in
              Ok (pad_bytes ^ promised_id_bytes ^ header_block_fragment ^ padding)
        )
    )
  | payload -> Error (PayloadMismatch { frame_type = Frame.PushPromise; payload })

let serialize_ping_payload = fun payload ->
  match payload with
  | Frame.PingPayload opaque_data ->
      let length = String.length opaque_data in
      if length != 8 then
        Error (InvalidPingPayloadLength { length })
      else
        Ok opaque_data
  | payload -> Error (PayloadMismatch { frame_type = Frame.Ping; payload })

let serialize_goaway_payload = fun payload ->
  match payload with
  | Frame.GoawayPayload { last_stream_id; error_code; debug_data } -> (
      match validate_last_stream_id last_stream_id with
      | Error error -> Error error
      | Ok () -> (
          match validate_error_code error_code with
          | Error error -> Error error
          | Ok code -> Ok (write_uint32_be last_stream_id ^ write_uint32_be code ^ debug_data)
        )
    )
  | payload -> Error (PayloadMismatch { frame_type = Frame.Goaway; payload })

let serialize_window_update_payload = fun payload ->
  match payload with
  | Frame.WindowUpdatePayload increment ->
      if increment <= 0 || increment > 0x7fff_ffff then
        Error (InvalidWindowUpdateIncrement { increment })
      else
        Ok (write_uint32_be increment)
  | payload -> Error (PayloadMismatch { frame_type = Frame.WindowUpdate; payload })

let serialize_continuation_payload = fun payload ->
  match payload with
  | Frame.ContinuationPayload header_block_fragment -> Ok header_block_fragment
  | payload -> Error (PayloadMismatch { frame_type = Frame.Continuation; payload })

let serialize_unknown_payload = fun frame_type payload ->
  match payload with
  | Frame.UnknownPayload data -> Ok data
  | payload -> Error (PayloadMismatch { frame_type; payload })

let serialize_payload = fun ~stream_id frame_type payload ->
  match frame_type with
  | Frame.Data -> serialize_data_payload payload
  | Frame.Headers -> serialize_headers_payload ~stream_id payload
  | Frame.Priority -> serialize_priority_payload ~stream_id payload
  | Frame.RstStream -> serialize_rst_stream_payload payload
  | Frame.Settings -> serialize_settings_payload payload
  | Frame.PushPromise -> serialize_push_promise_payload payload
  | Frame.Ping -> serialize_ping_payload payload
  | Frame.Goaway -> serialize_goaway_payload payload
  | Frame.WindowUpdate -> serialize_window_update_payload payload
  | Frame.Continuation -> serialize_continuation_payload payload
  | Frame.Unknown _ -> serialize_unknown_payload frame_type payload

let serialize_frame = fun frame ->
  let open Frame in
  let payload_bytes =
    match validate_stream_id frame.frame_type frame.stream_id with
    | Error error -> Error error
    | Ok () -> (
        match (frame.frame_type, frame.flags.ack) with
        | (Settings, true) -> (
            match frame.payload with
            | SettingsPayload [] -> Ok ""
            | SettingsPayload settings ->
                Error (SettingsAckWithPayload { setting_count = List.length settings })
            | payload -> Error (PayloadMismatch { frame_type = Settings; payload })
          )
        | _ -> serialize_payload ~stream_id:frame.stream_id frame.frame_type frame.payload
      )
  in
  match payload_bytes with
  | Error error -> Error error
  | Ok payload_bytes ->
      let payload_length = String.length payload_bytes in
      let max_length = 0x00ff_ffff in
      if payload_length > max_length then
        Error (PayloadLengthTooLarge { length = payload_length; max_length })
      else
        match frame_type_to_int frame.frame_type with
        | Error error -> Error error
        | Ok frame_type_code ->
            let length_bytes = write_uint24_be payload_length in
            let type_byte = write_uint8 frame_type_code in
            let flags_byte = write_uint8 (flags_to_byte frame.frame_type frame.flags) in
            let stream_id_bytes = write_uint32_be (frame.stream_id land 0x7fff_ffff) in
            Ok (length_bytes ^ type_byte ^ flags_byte ^ stream_id_bytes ^ payload_bytes)
