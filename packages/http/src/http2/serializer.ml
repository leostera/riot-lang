open Std

type payload_error = {
  frame_type: Frame.frame_type;
  payload: Frame.payload;
}

type error =
  | PayloadMismatch of payload_error
  | SettingsAckWithPayload of { setting_count: int }
  | InvalidPingPayloadLength of { length: int }
  | InvalidWindowUpdateIncrement of { increment: int }

let frame_type_to_string = function
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

let error_to_string = function
  | PayloadMismatch { frame_type; _ } ->
      "Payload does not match HTTP/2 " ^ frame_type_to_string frame_type ^ " frame"
  | SettingsAckWithPayload { setting_count } ->
      "SETTINGS ACK must not carry settings, got " ^ Int.to_string setting_count ^ " setting(s)"
  | InvalidPingPayloadLength { length } ->
      "PING opaque data must be exactly 8 bytes, got " ^ Int.to_string length
  | InvalidWindowUpdateIncrement { increment } ->
      "WINDOW_UPDATE increment must be between 1 and 2^31-1, got " ^ Int.to_string increment

let write_uint24_be = fun value ->
  let b0 = Char.from_int_unchecked ((value lsr 16) land 0xff) in
  let b1 = Char.from_int_unchecked ((value lsr 8) land 0xff) in
  let b2 = Char.from_int_unchecked (value land 0xff) in
  String.make ~len:1 ~char:b0 ^ String.make ~len:1 ~char:b1 ^ String.make ~len:1 ~char:b2

let write_uint32_be = fun value ->
  let b0 = Char.from_int_unchecked ((value lsr 24) land 0xff) in
  let b1 = Char.from_int_unchecked ((value lsr 16) land 0xff) in
  let b2 = Char.from_int_unchecked ((value lsr 8) land 0xff) in
  let b3 = Char.from_int_unchecked (value land 0xff) in
  String.make ~len:1 ~char:b0
  ^ String.make ~len:1 ~char:b1
  ^ String.make ~len:1 ~char:b2
  ^ String.make ~len:1 ~char:b3

let write_uint16_be = fun value ->
  let b0 = Char.from_int_unchecked ((value lsr 8) land 0xff) in
  let b1 = Char.from_int_unchecked (value land 0xff) in
  String.make ~len:1 ~char:b0 ^ String.make ~len:1 ~char:b1

let write_uint8 = fun value -> String.make ~len:1 ~char:(Char.from_int_unchecked (value land 0xff))

let frame_type_to_int = function
  | Frame.Data -> 0x0
  | Frame.Headers -> 0x1
  | Frame.Priority -> 0x2
  | Frame.RstStream -> 0x3
  | Frame.Settings -> 0x4
  | Frame.PushPromise -> 0x5
  | Frame.Ping -> 0x6
  | Frame.Goaway -> 0x7
  | Frame.WindowUpdate -> 0x8
  | Frame.Continuation -> 0x9
  | Frame.Unknown code -> code land 0xff

let flags_to_byte = fun frame_type flags ->
  let open Frame in
  let byte = 0 in
  let byte =
    if flags.end_stream then
      byte lor 0x01
    else
      byte
  in
  let byte =
    if flags.end_headers then
      byte lor 0x04
    else
      byte
  in
  let byte =
    if flags.padded then
      byte lor 0x08
    else
      byte
  in
  let byte =
    if flags.priority then
      byte lor 0x20
    else
      byte
  in
  let byte =
    if flags.ack then
      byte lor 0x01
    else
      byte
  in
  byte

let serialize_priority = fun stream_dependency exclusive weight ->
  let dep_with_exclusive =
    if exclusive then
      stream_dependency lor 0x8000_0000
    else
      stream_dependency land 0x7fff_ffff
  in
  write_uint32_be dep_with_exclusive ^ write_uint8 (weight - 1)

let serialize_data_payload = fun payload ->
  match payload with
  | Frame.DataPayload { data; pad_length = None } -> Ok data
  | Frame.DataPayload { data; pad_length = Some pad_len } ->
      Ok (write_uint8 pad_len ^ data ^ String.make ~len:pad_len ~char:'\x00')
  | payload -> Error (PayloadMismatch { frame_type = Frame.Data; payload })

let serialize_headers_payload = fun payload ->
  match payload with
  | Frame.HeadersPayload {
    pad_length;
    stream_dependency;
    weight;
    exclusive;
    header_block_fragment
  } ->
      let pad_bytes =
        match pad_length with
        | Some pl -> write_uint8 pl
        | None -> ""
      in
      let priority_bytes =
        match (stream_dependency, weight) with
        | (Some dep, Some w) -> serialize_priority dep exclusive w
        | _ -> ""
      in
      let padding =
        match pad_length with
        | Some pl -> String.make ~len:pl ~char:'\x00'
        | None -> ""
      in
      Ok (pad_bytes ^ priority_bytes ^ header_block_fragment ^ padding)
  | payload -> Error (PayloadMismatch { frame_type = Frame.Headers; payload })

let serialize_priority_payload = fun payload ->
  match payload with
  | Frame.PriorityPayload { stream_dependency; exclusive; weight } ->
      Ok (serialize_priority stream_dependency exclusive weight)
  | payload -> Error (PayloadMismatch { frame_type = Frame.Priority; payload })

let serialize_rst_stream_payload = fun payload ->
  match payload with
  | Frame.RstStreamPayload error_code -> Ok (write_uint32_be (Frame.error_code_to_int error_code))
  | payload -> Error (PayloadMismatch { frame_type = Frame.RstStream; payload })

let serialize_setting = function
  | Frame.HeaderTableSize value -> write_uint16_be 0x1 ^ write_uint32_be value
  | Frame.EnablePush enabled ->
      write_uint16_be 0x2 ^ write_uint32_be
        (
          if enabled then
            1
          else
            0
        )
  | Frame.MaxConcurrentStreams value -> write_uint16_be 0x3 ^ write_uint32_be value
  | Frame.InitialWindowSize value -> write_uint16_be 0x4 ^ write_uint32_be value
  | Frame.MaxFrameSize value -> write_uint16_be 0x5 ^ write_uint32_be value
  | Frame.MaxHeaderListSize value -> write_uint16_be 0x6 ^ write_uint32_be value

let serialize_settings_payload = fun payload ->
  match payload with
  | Frame.SettingsPayload settings ->
      Ok (String.concat "" (List.map settings ~fn:serialize_setting))
  | payload -> Error (PayloadMismatch { frame_type = Frame.Settings; payload })

let serialize_push_promise_payload = fun payload ->
  match payload with
  | Frame.PushPromisePayload { pad_length; promised_stream_id; header_block_fragment } ->
      let pad_bytes =
        match pad_length with
        | Some pl -> write_uint8 pl
        | None -> ""
      in
      let promised_id_bytes = write_uint32_be (promised_stream_id land 0x7fff_ffff) in
      let padding =
        match pad_length with
        | Some pl -> String.make ~len:pl ~char:'\x00'
        | None -> ""
      in
      Ok (pad_bytes ^ promised_id_bytes ^ header_block_fragment ^ padding)
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
  | Frame.GoawayPayload { last_stream_id; error_code; debug_data } ->
      Ok (write_uint32_be (last_stream_id land 0x7fff_ffff)
      ^ write_uint32_be (Frame.error_code_to_int error_code)
      ^ debug_data)
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

let serialize_payload = fun frame_type payload ->
  match frame_type with
  | Frame.Data -> serialize_data_payload payload
  | Frame.Headers -> serialize_headers_payload payload
  | Frame.Priority -> serialize_priority_payload payload
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
    match (frame.frame_type, frame.flags.ack) with
    | (Settings, true) -> (
        match frame.payload with
        | SettingsPayload [] -> Ok ""
        | SettingsPayload settings ->
            Error (SettingsAckWithPayload { setting_count = List.length settings })
        | payload -> Error (PayloadMismatch { frame_type = Settings; payload })
      )
    | _ -> serialize_payload frame.frame_type frame.payload
  in
  match payload_bytes with
  | Error error -> Error error
  | Ok payload_bytes ->
      let length_bytes = write_uint24_be (String.length payload_bytes) in
      let type_byte = write_uint8 (frame_type_to_int frame.frame_type) in
      let flags_byte = write_uint8 (flags_to_byte frame.frame_type frame.flags) in
      let stream_id_bytes = write_uint32_be (frame.stream_id land 0x7fff_ffff) in
      Ok (length_bytes ^ type_byte ^ flags_byte ^ stream_id_bytes ^ payload_bytes)
