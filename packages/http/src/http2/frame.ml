open Std

type frame_type =
  | Data
  | Headers
  | Priority
  | RstStream
  | Settings
  | PushPromise
  | Ping
  | Goaway
  | WindowUpdate
  | Continuation
  | Unknown of int

type flags = { end_stream: bool; end_headers: bool; padded: bool; priority: bool; ack: bool }

type stream_id = int

type error_code =
  | NoError
  | ProtocolError
  | InternalError
  | FlowControlError
  | SettingsTimeout
  | StreamClosed
  | FrameSizeError
  | RefusedStream
  | Cancel
  | CompressionError
  | ConnectError
  | EnhanceYourCalm
  | InadequateSecurity
  | Http11Required
  | UnknownErrorCode of int

type setting =
  | HeaderTableSize of int
  | EnablePush of bool
  | MaxConcurrentStreams of int
  | InitialWindowSize of int
  | MaxFrameSize of int
  | MaxHeaderListSize of int

type payload =
  | DataPayload of {
      data: string;
      pad_length: int option;
    }
  | HeadersPayload of {
      pad_length: int option;
      stream_dependency: int option;
      weight: int option;
      exclusive: bool;
      header_block_fragment: string;
    }
  | PriorityPayload of { stream_dependency: int; exclusive: bool; weight: int }
  | RstStreamPayload of error_code
  | SettingsPayload of setting list
  | PushPromisePayload of {
      pad_length: int option;
      promised_stream_id: int;
      header_block_fragment: string;
    }
  | PingPayload of string
  | GoawayPayload of {
      last_stream_id: int;
      error_code: error_code;
      debug_data: string;
    }
  | WindowUpdatePayload of int
  | ContinuationPayload of string
  | UnknownPayload of string

type t = {
  length: int;
  frame_type: frame_type;
  flags: flags;
  stream_id: stream_id;
  payload: payload;
}

type constructor_error =
  | InvalidPingPayloadLength of { length: int }
  | InvalidWindowUpdateIncrement of { increment: int }

let constructor_error_to_string = fun __tmp1 ->
  match __tmp1 with
  | InvalidPingPayloadLength { length } ->
      "PING opaque data must be exactly 8 bytes, got " ^ Int.to_string length
  | InvalidWindowUpdateIncrement { increment } ->
      "WINDOW_UPDATE increment must be between 1 and 2^31-1, got " ^ Int.to_string increment

let default_flags = {
  end_stream = false;
  end_headers = false;
  padded = false;
  priority = false;
  ack = false;
}

let data = fun ~stream_id ?(end_stream = false) ?pad_length data ->
  let flags = { default_flags with end_stream; padded = Option.is_some pad_length } in
  let payload_len =
    String.length data + match pad_length with
    | Some n -> n + 1
    | None -> 0
  in
  {
    length = payload_len;
    frame_type = Data;
    flags;
    stream_id;
    payload = DataPayload { data; pad_length };
  }

let headers = fun
  ~stream_id
  ?(end_stream = false)
  ?(end_headers = false)
  ?pad_length
  ?priority
  header_block_fragment ->
  let has_priority = Option.is_some priority in
  let flags = {
    default_flags with
    end_stream;
    end_headers;
    padded = Option.is_some pad_length;
    priority = has_priority;
  }
  in
  let (stream_dependency, weight, exclusive) =
    match priority with
    | Some (dep, excl, w) -> (Some dep, Some w, excl)
    | None -> (None, None, false)
  in
  let payload_len =
    String.length header_block_fragment + (
      match pad_length with
      | Some n -> n + 1
      | None -> 0
    ) + if has_priority then
      5
    else
      0
  in
  {
    length = payload_len;
    frame_type = Headers;
    flags;
    stream_id;
    payload =
      HeadersPayload {
        pad_length;
        stream_dependency;
        weight;
        exclusive;
        header_block_fragment;
      };
  }

let priority = fun ~stream_id ~stream_dependency ~exclusive ~weight ->
  {
    length = 5;
    frame_type = Priority;
    flags = default_flags;
    stream_id;
    payload = PriorityPayload { stream_dependency; exclusive; weight };
  }

let rst_stream = fun ~stream_id error_code ->
  {
    length = 4;
    frame_type = RstStream;
    flags = default_flags;
    stream_id;
    payload = RstStreamPayload error_code;
  }

let settings = fun ?(ack = false) settings_list ->
  let flags = { default_flags with ack } in
  let length =
    if ack then
      0
    else
      List.length settings_list * 6
  in
  {
    length;
    frame_type = Settings;
    flags;
    stream_id = 0;
    payload = SettingsPayload settings_list;
  }

let push_promise = fun ~stream_id ~promised_stream_id ?pad_length header_block_fragment ->
  let flags = { default_flags with padded = Option.is_some pad_length } in
  let payload_len =
    4 + String.length header_block_fragment + match pad_length with
    | Some n -> n + 1
    | None -> 0
  in
  {
    length = payload_len;
    frame_type = PushPromise;
    flags;
    stream_id;
    payload = PushPromisePayload { pad_length; promised_stream_id; header_block_fragment };
  }

let ping = fun ?(ack = false) opaque_data ->
  let length = String.length opaque_data in
  if length != 8 then
    Error (InvalidPingPayloadLength { length })
  else
    let flags = { default_flags with ack } in
    Ok {
      length = 8;
      frame_type = Ping;
      flags;
      stream_id = 0;
      payload = PingPayload opaque_data;
    }

let goaway = fun ~last_stream_id ~error_code ?(debug_data = "") () ->
  {
    length = 8 + String.length debug_data;
    frame_type = Goaway;
    flags = default_flags;
    stream_id = 0;
    payload = GoawayPayload { last_stream_id; error_code; debug_data };
  }

let window_update = fun ~stream_id increment ->
  if increment <= 0 || increment > 0x7fff_ffff then
    Error (InvalidWindowUpdateIncrement { increment })
  else
    Ok {
      length = 4;
      frame_type = WindowUpdate;
      flags = default_flags;
      stream_id;
      payload = WindowUpdatePayload increment;
    }

let continuation = fun ~stream_id ?(end_headers = false) header_block_fragment ->
  let flags = { default_flags with end_headers } in
  {
    length = String.length header_block_fragment;
    frame_type = Continuation;
    flags;
    stream_id;
    payload = ContinuationPayload header_block_fragment;
  }

let error_code_to_int = fun __tmp1 ->
  match __tmp1 with
  | NoError -> 0x0
  | ProtocolError -> 0x1
  | InternalError -> 0x2
  | FlowControlError -> 0x3
  | SettingsTimeout -> 0x4
  | StreamClosed -> 0x5
  | FrameSizeError -> 0x6
  | RefusedStream -> 0x7
  | Cancel -> 0x8
  | CompressionError -> 0x9
  | ConnectError -> 0xa
  | EnhanceYourCalm -> 0xb
  | InadequateSecurity -> 0xc
  | Http11Required -> 0xd
  | UnknownErrorCode code -> code

let int_to_error_code = fun __tmp1 ->
  match __tmp1 with
  | 0x0 -> Some NoError
  | 0x1 -> Some ProtocolError
  | 0x2 -> Some InternalError
  | 0x3 -> Some FlowControlError
  | 0x4 -> Some SettingsTimeout
  | 0x5 -> Some StreamClosed
  | 0x6 -> Some FrameSizeError
  | 0x7 -> Some RefusedStream
  | 0x8 -> Some Cancel
  | 0x9 -> Some CompressionError
  | 0xa -> Some ConnectError
  | 0xb -> Some EnhanceYourCalm
  | 0xc -> Some InadequateSecurity
  | 0xd -> Some Http11Required
  | code when code >= 0 && code <= 0xffff_ffff -> Some (UnknownErrorCode code)
  | _ -> None
