open Std
open Std.IO
open Std.Collections

(* Use Buffer from Std.IO *)

module Buffer = IO.Buffer

(* Use Cell from Sync *)

module Cell = Sync.Cell

(** HTTP/2 Connection Management (RFC 9113) *)
type state =
  | Idle
  | Active
  | GoingAway
  | Closed

type stream_state =
  | StreamIdle
  | StreamOpen
  | StreamReservedLocal
  | StreamReservedRemote
  | StreamHalfClosedLocal
  | StreamHalfClosedRemote
  | StreamClosed

type stream = {
  id: int;
  state: stream_state Cell.t;
  window_size: int Cell.t;
  receive_window_size: int Cell.t;
  headers: Hpack.header list Cell.t;
  data_chunks: bytes list Cell.t;
}

type pending_header_block = {
  stream_id: int;
  fragments: string list;
  end_stream: bool;
}

type preface_state =
  | AwaitingClientPreface of string
  | AwaitingInitialSettings
  | PrefaceComplete

type settings = {
  header_table_size: int Cell.t;
  enable_push: bool Cell.t;
  max_concurrent_streams: int option Cell.t;
  initial_window_size: int Cell.t;
  max_frame_size: int Cell.t;
  max_header_list_size: int option Cell.t;
}

type config = {
  max_frame_size: int;
  initial_window_size: int;
  max_concurrent_streams: int;
  enable_push: bool;
}

type role =
  | Client
  | Server

type t = {
  role: role;
  state: state Cell.t;
  streams: (int, stream) HashMap.t;
  next_stream_id: int Cell.t;
  send_connection_window_size: int Cell.t;
  receive_connection_window_size: int Cell.t;
  local_settings: settings;
  remote_settings: settings;
  hpack_encoder: Hpack.encoder;
  hpack_decoder: Hpack.decoder;
  last_stream_id: int Cell.t;
  (** Last stream ID we initiated *)
  peer_last_stream_id: int Cell.t;
  (** Last stream ID peer initiated *)
  pending_input: string Cell.t;
  pending_header_block: pending_header_block option Cell.t;
  preface_state: preface_state Cell.t;
}

type event =
  | HeadersReceived of {
      stream_id: int;
      headers: Hpack.header list;
      end_stream: bool;
    }
  | DataReceived of { stream_id: int; data: bytes; end_stream: bool }
  | SettingsReceived of Frame.setting list
  | SettingsAckReceived
  | PingReceived of { data: string }
  | PingAckReceived of { data: string }
  | GoawayReceived of {
      last_stream_id: int;
      error_code: Frame.error_code;
      debug_data: string;
    }
  | RstStreamReceived of {
      stream_id: int;
      error_code: Frame.error_code;
    }
  | WindowUpdateReceived of { stream_id: int; increment: int }
  | PriorityReceived of { stream_id: int; stream_dependency: int; weight: int; exclusive: bool }

type window_scope =
  | StreamWindow of { stream_id: int }
  | ConnectionWindow

type stream_initiator =
  | LocalInitiated
  | PeerInitiated

type payload_error = {
  frame_type: Frame.frame_type;
  payload: Frame.payload;
}

type error =
  | ConnectionNotActive
  | StreamNotFound of { stream_id: int }
  | FlowControlWindowExceeded of {
      scope: window_scope;
      data_size: int;
      window_size: int;
    }
  | FlowControlWindowOverflow of {
      scope: window_scope;
      increment: int;
      window_size: int;
      max_size: int;
    }
  | MaxConcurrentStreamsExceeded of {
      initiator: stream_initiator;
      stream_id: int;
      current: int;
      limit: int;
    }
  | HpackEncodeFailed of Hpack.encode_error
  | HpackDecodeFailed of Hpack.decode_error
  | HpackTableSizeUpdateFailed of Hpack.table_size_error
  | InvalidPayloadForFrame of payload_error
  | UnsupportedFrameReceived of payload_error
  | ExpectedContinuation of {
      stream_id: int;
      frame_type: Frame.frame_type;
    }
  | UnexpectedContinuation of { stream_id: int }
  | ContinuationStreamMismatch of { expected_stream_id: int; actual_stream_id: int }
  | InvalidPeerStreamId of {
      role: role;
      stream_id: int;
    }
  | PeerStreamIdNotIncreasing of { stream_id: int; last_stream_id: int }
  | NewStreamRejected of {
      state: state;
      stream_id: int;
    }
  | DataBeforeHeaders of { stream_id: int }
  | FrameForIdleStream of {
      stream_id: int;
      frame_type: Frame.frame_type;
    }
  | FrameAfterStreamEnd of {
      stream_id: int;
      frame_type: Frame.frame_type;
      state: stream_state;
    }
  | ParserError of Parser.error
  | FrameConstructorError of Frame.constructor_error
  | SerializerError of Serializer.error
  | InvalidClientPrefaceByte of { offset: int; expected: int; actual: int }
  | ExpectedInitialSettings of {
      frame_type: Frame.frame_type;
    }

let window_scope_to_string = function
  | StreamWindow { stream_id } -> "stream " ^ Int.to_string stream_id
  | ConnectionWindow -> "connection"

let role_to_string = function
  | Client -> "client"
  | Server -> "server"

let stream_initiator_to_string = function
  | LocalInitiated -> "local"
  | PeerInitiated -> "peer"

let state_to_string = function
  | Idle -> "idle"
  | Active -> "active"
  | GoingAway -> "going away"
  | Closed -> "closed"

let stream_state_to_string = function
  | StreamIdle -> "idle"
  | StreamOpen -> "open"
  | StreamReservedLocal -> "reserved local"
  | StreamReservedRemote -> "reserved remote"
  | StreamHalfClosedLocal -> "half-closed local"
  | StreamHalfClosedRemote -> "half-closed remote"
  | StreamClosed -> "closed"

let error_to_string = function
  | ConnectionNotActive -> "HTTP/2 connection is not active"
  | StreamNotFound { stream_id } -> "HTTP/2 stream not found: " ^ Int.to_string stream_id
  | FlowControlWindowExceeded { scope; data_size; window_size } ->
      "HTTP/2 DATA size "
      ^ Int.to_string data_size
      ^ " exceeds "
      ^ window_scope_to_string scope
      ^ " flow-control window "
      ^ Int.to_string window_size
  | FlowControlWindowOverflow {
      scope;
      increment;
      window_size;
      max_size;
    } ->
      "HTTP/2 WINDOW_UPDATE increment "
      ^ Int.to_string increment
      ^ " would overflow "
      ^ window_scope_to_string scope
      ^ " flow-control window "
      ^ Int.to_string window_size
      ^ " beyond "
      ^ Int.to_string max_size
  | MaxConcurrentStreamsExceeded {
      initiator;
      stream_id;
      current;
      limit;
    } ->
      "HTTP/2 "
      ^ stream_initiator_to_string initiator
      ^ " stream "
      ^ Int.to_string stream_id
      ^ " exceeds max concurrent streams "
      ^ Int.to_string limit
      ^ " with "
      ^ Int.to_string current
      ^ " stream(s) already active"
  | HpackEncodeFailed error -> "HPACK encode failed: " ^ Hpack.encode_error_to_string error
  | HpackDecodeFailed error -> "HPACK decode failed: " ^ Hpack.decode_error_to_string error
  | HpackTableSizeUpdateFailed error ->
      "HPACK table size update failed: " ^ Hpack.table_size_error_to_string error
  | InvalidPayloadForFrame { frame_type; _ } ->
      "Invalid payload for HTTP/2 " ^ Parser.frame_type_name frame_type ^ " frame"
  | UnsupportedFrameReceived { frame_type; _ } ->
      "Unsupported HTTP/2 " ^ Parser.frame_type_name frame_type ^ " frame"
  | ExpectedContinuation { stream_id; frame_type } ->
      "HTTP/2 expected CONTINUATION for stream "
      ^ Int.to_string stream_id
      ^ ", got "
      ^ Parser.frame_type_name frame_type
  | UnexpectedContinuation { stream_id } ->
      "HTTP/2 received CONTINUATION for stream "
      ^ Int.to_string stream_id
      ^ " without an open header block"
  | ContinuationStreamMismatch { expected_stream_id; actual_stream_id } ->
      "HTTP/2 expected CONTINUATION for stream "
      ^ Int.to_string expected_stream_id
      ^ ", got stream "
      ^ Int.to_string actual_stream_id
  | InvalidPeerStreamId { role; stream_id } ->
      "HTTP/2 "
      ^ role_to_string role
      ^ " connection received HEADERS for invalid peer-initiated stream "
      ^ Int.to_string stream_id
  | PeerStreamIdNotIncreasing { stream_id; last_stream_id } ->
      "HTTP/2 received new peer stream "
      ^ Int.to_string stream_id
      ^ " after peer stream "
      ^ Int.to_string last_stream_id
  | NewStreamRejected { state; stream_id } ->
      "HTTP/2 connection rejected new stream "
      ^ Int.to_string stream_id
      ^ " while "
      ^ state_to_string state
  | DataBeforeHeaders { stream_id } ->
      "HTTP/2 received DATA before headers on stream " ^ Int.to_string stream_id
  | FrameForIdleStream { stream_id; frame_type } ->
      "HTTP/2 received "
      ^ Parser.frame_type_name frame_type
      ^ " for idle stream "
      ^ Int.to_string stream_id
  | FrameAfterStreamEnd { stream_id; frame_type; state } ->
      "HTTP/2 received "
      ^ Parser.frame_type_name frame_type
      ^ " after stream "
      ^ Int.to_string stream_id
      ^ " reached "
      ^ stream_state_to_string state
  | ParserError error -> Parser.error_to_string error
  | FrameConstructorError error -> Frame.constructor_error_to_string error
  | SerializerError error -> Serializer.error_to_string error
  | InvalidClientPrefaceByte { offset; expected; actual } ->
      "HTTP/2 client preface byte "
      ^ Int.to_string offset
      ^ " was "
      ^ Int.to_string actual
      ^ ", expected "
      ^ Int.to_string expected
  | ExpectedInitialSettings { frame_type } ->
      "HTTP/2 expected initial SETTINGS frame, got " ^ Parser.frame_type_name frame_type

let frame_constructor_error = fun error -> FrameConstructorError error

let serialize_frame = fun frame ->
  Serializer.serialize_frame frame
  |> Result.map_err ~fn:(fun error -> SerializerError error)

(** {1 Constants} *)

(** RFC 9113: Connection preface for clients *)
let client_preface = "PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n"

let max_flow_control_window_size = 2_147_483_647

(** Default settings per RFC 9113 *)
let default_config = {
  max_frame_size = 16_384;
  initial_window_size = 65_535;
  max_concurrent_streams = 100;
  enable_push = false;
}

let create_settings = fun config ->
  {
    header_table_size = Cell.create 4_096;
    enable_push = Cell.create config.enable_push;
    max_concurrent_streams = Cell.create (Some config.max_concurrent_streams);
    initial_window_size = Cell.create config.initial_window_size;
    max_frame_size = Cell.create config.max_frame_size;
    max_header_list_size = Cell.create None;
  }

(** {1 Connection Lifecycle} *)

let create = fun ~role ?(config = default_config) () ->
  {
    role;
    state = Cell.create Idle;
    streams = HashMap.with_capacity ~size:16;
    next_stream_id =
      Cell.create
        (
          match role with
          | Client -> 1
          | Server -> 2
        );
    send_connection_window_size = Cell.create 65_535;
    receive_connection_window_size = Cell.create 65_535;
    local_settings = create_settings config;
    remote_settings = create_settings config;
    hpack_encoder = Hpack.create_encoder ~max_dynamic_table_size:4_096 ();
    hpack_decoder = Hpack.create_decoder ~max_dynamic_table_size:4_096 ();
    last_stream_id = Cell.create 0;
    peer_last_stream_id = Cell.create 0;
    pending_input = Cell.create "";
    pending_header_block = Cell.create None;
    preface_state =
      Cell.create
        (
          match role with
          | Server -> AwaitingClientPreface ""
          | Client -> AwaitingInitialSettings
        );
  }

let send_preface = fun conn ->
  (* Create initial SETTINGS frame *)
  let settings_list = [
    Frame.HeaderTableSize (Cell.get conn.local_settings.header_table_size);
    Frame.EnablePush (Cell.get conn.local_settings.enable_push);
    Frame.MaxConcurrentStreams (Option.unwrap_or
      ~default:100
      (Cell.get conn.local_settings.max_concurrent_streams));
    Frame.InitialWindowSize (Cell.get conn.local_settings.initial_window_size);
    Frame.MaxFrameSize (Cell.get conn.local_settings.max_frame_size);
  ]
  in
  let settings_frame = Frame.settings settings_list in
  match serialize_frame settings_frame with
  | Error error -> Error error
  | Ok settings_bytes -> (
      match conn.role with
      | Client ->
          (* Client sends preface string + SETTINGS *)
          Cell.set conn.state Active;
          Ok (client_preface ^ settings_bytes)
      | Server ->
          (* Server sends only SETTINGS *)
          Cell.set conn.state Active;
          Ok settings_bytes
    )

(** {1 Stream Management} *)

let is_valid_stream_id = fun ~role stream_id ->
  match role with
  | Client -> stream_id mod 2 = 1
  | Server -> stream_id mod 2 = 0

(* Server streams are even *)

let is_valid_peer_initiated_stream_id = fun ~role stream_id ->
  match role with
  | Server -> stream_id mod 2 = 1
  | Client -> stream_id mod 2 = 0

let stream_matches_initiator = fun ~role initiator stream_id ->
  match initiator with
  | LocalInitiated -> is_valid_stream_id ~role stream_id
  | PeerInitiated -> is_valid_peer_initiated_stream_id ~role stream_id

let count_active_streams = fun conn ~initiator ->
  HashMap.fold_left
    conn.streams
    ~init:0
    ~fn:(fun count stream_id stream ->
      if
        stream_matches_initiator ~role:conn.role initiator stream_id
        && Cell.get stream.state != StreamClosed
      then
        count + 1
      else
        count)

let ensure_stream_capacity = fun conn ~initiator ~stream_id ~limit ->
  match limit with
  | None -> Ok ()
  | Some limit ->
      let current = count_active_streams conn ~initiator in
      if current >= limit then
        Error (
          MaxConcurrentStreamsExceeded {
            initiator;
            stream_id;
            current;
            limit;
          }
        )
      else
        Ok ()

let ensure_accepts_new_peer_stream = fun conn ~stream_id ->
  match Cell.get conn.state with
  | GoingAway -> Error (NewStreamRejected { state = GoingAway; stream_id })
  | Closed -> Error (NewStreamRejected { state = Closed; stream_id })
  | Idle
  | Active -> Ok ()

let ensure_peer_stream_id_increases = fun conn ~stream_id ->
  let last_stream_id = Cell.get conn.peer_last_stream_id in
  if stream_id <= last_stream_id then
    Error (PeerStreamIdNotIncreasing { stream_id; last_stream_id })
  else
    Ok ()

let create_stream = fun conn ->
  if Cell.get conn.state != Active then
    Error ConnectionNotActive
  else
    let stream_id = Cell.get conn.next_stream_id in
    match ensure_stream_capacity
      conn
      ~initiator:LocalInitiated
      ~stream_id
      ~limit:(Cell.get conn.remote_settings.max_concurrent_streams) with
    | Error error -> Error error
    | Ok () ->
        Cell.set conn.next_stream_id (stream_id + 2);
        (* Skip to next valid ID *)
        Cell.set conn.last_stream_id stream_id;
        let stream = {
          id = stream_id;
          state = Cell.create StreamIdle;
          window_size = Cell.create (Cell.get conn.remote_settings.initial_window_size);
          receive_window_size = Cell.create (Cell.get conn.local_settings.initial_window_size);
          headers = Cell.create [];
          data_chunks = Cell.create [];
        }
        in
        let _ = HashMap.insert conn.streams ~key:stream_id ~value:stream in
        Ok stream_id

let get_stream = fun conn stream_id -> HashMap.get conn.streams ~key:stream_id

let transition_send_headers = fun ~stream_id state ~end_stream ->
  match state with
  | StreamIdle
  | StreamOpen ->
      Ok (
        if end_stream then
          StreamHalfClosedLocal
        else
          StreamOpen
      )
  | StreamHalfClosedRemote ->
      Ok (
        if end_stream then
          StreamClosed
        else
          StreamHalfClosedRemote
      )
  | StreamReservedLocal ->
      Ok (
        if end_stream then
          StreamHalfClosedLocal
        else
          StreamOpen
      )
  | StreamReservedRemote -> Error (FrameForIdleStream { stream_id; frame_type = Frame.Headers })
  | StreamHalfClosedLocal
  | StreamClosed -> Error (FrameAfterStreamEnd { stream_id; frame_type = Frame.Headers; state })

let transition_send_data = fun ~stream_id state ~end_stream ->
  match state with
  | StreamIdle -> Error (DataBeforeHeaders { stream_id })
  | StreamOpen ->
      Ok (
        if end_stream then
          StreamHalfClosedLocal
        else
          StreamOpen
      )
  | StreamHalfClosedRemote ->
      Ok (
        if end_stream then
          StreamClosed
        else
          StreamHalfClosedRemote
      )
  | StreamReservedLocal
  | StreamReservedRemote -> Error (FrameForIdleStream { stream_id; frame_type = Frame.Data })
  | StreamHalfClosedLocal
  | StreamClosed -> Error (FrameAfterStreamEnd { stream_id; frame_type = Frame.Data; state })

let transition_recv_headers = fun ~stream_id state ~end_stream ->
  match state with
  | StreamIdle
  | StreamOpen ->
      Ok (
        if end_stream then
          StreamHalfClosedRemote
        else
          StreamOpen
      )
  | StreamHalfClosedLocal ->
      Ok (
        if end_stream then
          StreamClosed
        else
          StreamHalfClosedLocal
      )
  | StreamReservedRemote ->
      Ok (
        if end_stream then
          StreamHalfClosedRemote
        else
          StreamOpen
      )
  | StreamReservedLocal -> Error (FrameForIdleStream { stream_id; frame_type = Frame.Headers })
  | StreamHalfClosedRemote
  | StreamClosed -> Error (FrameAfterStreamEnd { stream_id; frame_type = Frame.Headers; state })

let transition_recv_data = fun ~stream_id state ~end_stream ->
  match state with
  | StreamIdle -> Error (DataBeforeHeaders { stream_id })
  | StreamOpen ->
      Ok (
        if end_stream then
          StreamHalfClosedRemote
        else
          StreamOpen
      )
  | StreamHalfClosedLocal ->
      Ok (
        if end_stream then
          StreamClosed
        else
          StreamHalfClosedLocal
      )
  | StreamReservedLocal
  | StreamReservedRemote -> Error (FrameForIdleStream { stream_id; frame_type = Frame.Data })
  | StreamHalfClosedRemote
  | StreamClosed -> Error (FrameAfterStreamEnd { stream_id; frame_type = Frame.Data; state })

let serialize_frames = fun frames ->
  let buf = Buffer.create ~size:256 in
  let rec loop = function
    | [] ->
        Ok (Buffer.contents buf)
    | frame :: rest -> (
        match serialize_frame frame with
        | Error error -> Error error
        | Ok bytes ->
            Buffer.add_string buf bytes;
            loop rest
      )
  in
  loop frames

let split_string_chunks = fun ~max_size data ->
  let max_size =
    if max_size <= 0 then
      1
    else
      max_size
  in
  let len = String.length data in
  if len = 0 then
    [ "" ]
  else
    let rec loop offset acc =
      if offset >= len then
        List.reverse acc
      else
        let remaining = len - offset in
        let chunk_len =
          if remaining > max_size then
            max_size
          else
            remaining
        in
        loop (offset + chunk_len) (String.sub data ~offset ~len:chunk_len :: acc)
    in
    loop 0 []

let send_headers = fun conn ~stream_id ~headers ~end_stream ->
  match get_stream conn stream_id with
  | None -> Error (StreamNotFound { stream_id })
  | Some stream -> (
      match Hpack.encode conn.hpack_encoder ~sensitive_headers:[] () ~headers with
      | Error error -> Error (HpackEncodeFailed error)
      | Ok encoded_headers ->
          let state = Cell.get stream.state in
          match transition_send_headers ~stream_id state ~end_stream with
          | Error error -> Error error
          | Ok next_state ->
              let max_frame_size = Cell.get conn.remote_settings.max_frame_size in
              let header_block = Bytes.to_string encoded_headers in
              let chunks = split_string_chunks ~max_size:max_frame_size header_block in
              let frames =
                match chunks with
                | [] -> []
                | first :: rest ->
                    let headers_frame =
                      Frame.headers ~stream_id ~end_stream ~end_headers:(rest = []) first
                    in
                    let rec continuations acc = function
                      | [] -> List.reverse acc
                      | [ last ] ->
                          List.reverse (Frame.continuation ~stream_id ~end_headers:true last :: acc)
                      | chunk :: tail ->
                          continuations
                            (Frame.continuation ~stream_id ~end_headers:false chunk :: acc)
                            tail
                    in
                    headers_frame :: continuations [] rest
              in
              match serialize_frames frames with
              | Error error -> Error error
              | Ok bytes ->
                  Cell.set stream.state next_state;
                  Ok bytes
    )

let send_data = fun conn ~stream_id ~data ~end_stream ->
  match get_stream conn stream_id with
  | None -> Error (StreamNotFound { stream_id })
  | Some stream -> (
      let data_len = Bytes.length data in
      (* Check flow control window *)
      let stream_window = Cell.get stream.window_size in
      let conn_window = Cell.get conn.send_connection_window_size in
      if data_len > stream_window then
        Error (FlowControlWindowExceeded {
          scope = StreamWindow { stream_id };
          data_size = data_len;
          window_size = stream_window;
        })
      else if data_len > conn_window then
        Error (FlowControlWindowExceeded {
          scope = ConnectionWindow;
          data_size = data_len;
          window_size = conn_window;
        })
      else
        let state = Cell.get stream.state in
        match transition_send_data ~stream_id state ~end_stream with
        | Error error -> Error error
        | Ok next_state ->
            let max_frame_size = Cell.get conn.remote_settings.max_frame_size in
            let chunks = split_string_chunks ~max_size:max_frame_size (Bytes.to_string data) in
            let rec build_frames acc = function
              | [] -> List.reverse acc
              | [ last ] ->
                  build_frames
                    (
                      {
                        Frame.length = String.length last;
                        frame_type = Frame.Data;
                        flags =
                          {
                            Frame.end_stream;
                            end_headers = false;
                            padded = false;
                            priority = false;
                            ack = false;
                          };
                        stream_id;
                        payload = Frame.DataPayload { data = last; pad_length = None };
                      }
                      :: acc
                    )
                    []
              | chunk :: rest ->
                  build_frames
                    (
                      {
                        Frame.length = String.length chunk;
                        frame_type = Frame.Data;
                        flags =
                          {
                            Frame.end_stream = false;
                            end_headers = false;
                            padded = false;
                            priority = false;
                            ack = false;
                          };
                        stream_id;
                        payload = Frame.DataPayload { data = chunk; pad_length = None };
                      }
                      :: acc
                    )
                    rest
            in
            let frames = build_frames [] chunks in
            match serialize_frames frames with
            | Error error -> Error error
            | Ok bytes ->
                Cell.set stream.window_size (stream_window - data_len);
                Cell.set conn.send_connection_window_size (conn_window - data_len);
                Cell.set stream.state next_state;
                Ok bytes
    )

let reset_stream = fun conn ~stream_id ~error_code ->
  let frame = {
    Frame.length = 4;
    frame_type = Frame.RstStream;
    flags =
      {
        Frame.end_stream = false;
        end_headers = false;
        padded = false;
        priority = false;
        ack = false;
      };
    stream_id;
    payload = Frame.RstStreamPayload error_code;
  }
  in
  (
    match get_stream conn stream_id with
    | Some stream -> Cell.set stream.state StreamClosed
    | None -> ()
  );
  serialize_frame frame

(** {1 Flow Control} *)

let add_flow_control_window = fun cell ~scope ~increment ->
  let window_size = Cell.get cell in
  if increment > max_flow_control_window_size - window_size then
    Error (
      FlowControlWindowOverflow {
        scope;
        increment;
        window_size;
        max_size = max_flow_control_window_size;
      }
    )
  else (
    Cell.set cell (window_size + increment);
    Ok ()
  )

let send_window_update_connection = fun conn ~increment ->
  match Frame.window_update ~stream_id:0 increment with
  | Error error -> Error (frame_constructor_error error)
  | Ok frame -> (
      match add_flow_control_window
        conn.receive_connection_window_size
        ~scope:ConnectionWindow
        ~increment with
      | Error error -> Error error
      | Ok () -> serialize_frame frame
    )

let send_window_update_stream = fun conn ~stream_id ~increment ->
  match Frame.window_update ~stream_id increment with
  | Error error -> Error (frame_constructor_error error)
  | Ok frame -> (
      match get_stream conn stream_id with
      | None -> Error (StreamNotFound { stream_id })
      | Some stream -> (
          match add_flow_control_window
            stream.receive_window_size
            ~scope:(StreamWindow { stream_id })
            ~increment with
          | Error error -> Error error
          | Ok () -> serialize_frame frame
        )
    )

let connection_window_size = fun conn -> Cell.get conn.send_connection_window_size

let receive_connection_window_size = fun conn -> Cell.get conn.receive_connection_window_size

let stream_window_size = fun conn ~stream_id ->
  match get_stream conn stream_id with
  | Some stream -> Some (Cell.get stream.window_size)
  | None -> None

let receive_stream_window_size = fun conn ~stream_id ->
  match get_stream conn stream_id with
  | Some stream -> Some (Cell.get stream.receive_window_size)
  | None -> None

(** {1 Settings} *)

let update_settings = fun conn settings ->
  let frame = {
    Frame.length = List.length settings * 6;
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
    payload = Frame.SettingsPayload settings;
  }
  in
  serialize_frame frame

let send_settings_ack = fun conn ->
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
  serialize_frame frame

let local_settings = fun conn -> conn.local_settings

let remote_settings = fun conn -> conn.remote_settings

(** {1 Connection Control} *)

let send_ping = fun conn ~data ->
  match Frame.ping data with
  | Error error -> Error (frame_constructor_error error)
  | Ok frame -> serialize_frame frame

let send_ping_ack = fun conn ~data ->
  match Frame.ping ~ack:true data with
  | Error error -> Error (frame_constructor_error error)
  | Ok frame -> serialize_frame frame

let send_goaway = fun conn ~last_stream_id ~error_code ?(debug_data = "") () ->
  Cell.set conn.state GoingAway;
  let frame = {
    Frame.length = 8 + String.length debug_data;
    frame_type = Frame.Goaway;
    flags =
      {
        Frame.end_stream = false;
        end_headers = false;
        padded = false;
        priority = false;
        ack = false;
      };
    stream_id = 0;
    payload = Frame.GoawayPayload { last_stream_id; error_code; debug_data };
  }
  in
  serialize_frame frame

let state = fun conn -> Cell.get conn.state

let close = fun conn ->
  Cell.set conn.state Closed;
  HashMap.clear conn.streams

(** {1 Frame Processing} *)

let process_settings_frame = fun conn settings_list flags ->
  if flags.Frame.ack then
    Ok [ SettingsAckReceived ]
  else
    let adjust_send_stream_windows = fun delta ->
      let _ =
        HashMap.fold_left
          conn.streams
          ~init:()
          ~fn:(fun () _stream_id stream ->
            Cell.set
              stream.window_size
              (Cell.get stream.window_size + delta))
      in
      ()
    in
    let rec apply_settings = function
      | [] ->
          Ok [ SettingsReceived settings_list ]
      | setting :: rest -> (
          match setting with
          | Frame.HeaderTableSize size -> (
              match Hpack.update_encoder_max_table_size conn.hpack_encoder size with
              | Error error -> Error (HpackTableSizeUpdateFailed error)
              | Ok () ->
                  Cell.set conn.remote_settings.header_table_size size;
                  apply_settings rest
            )
          | Frame.EnablePush enabled ->
              Cell.set conn.remote_settings.enable_push enabled;
              apply_settings rest
          | Frame.MaxConcurrentStreams max ->
              Cell.set conn.remote_settings.max_concurrent_streams (Some max);
              apply_settings rest
          | Frame.InitialWindowSize size ->
              let previous = Cell.get conn.remote_settings.initial_window_size in
              Cell.set conn.remote_settings.initial_window_size size;
              adjust_send_stream_windows (size - previous);
              apply_settings rest
          | Frame.MaxFrameSize size ->
              Cell.set conn.remote_settings.max_frame_size size;
              apply_settings rest
          | Frame.MaxHeaderListSize size ->
              Cell.set conn.remote_settings.max_header_list_size (Some size);
              apply_settings rest
        )
    in
    apply_settings settings_list

let decode_header_block = fun conn ~stream_id ~fragment ~end_stream ->
  let header_bytes = Bytes.from_string fragment in
  match Hpack.decode conn.hpack_decoder header_bytes with
  | Error e -> Error (HpackDecodeFailed e)
  | Ok headers ->
      let stream =
        match get_stream conn stream_id with
        | Some stream -> (
            let state = Cell.get stream.state in
            match transition_recv_headers ~stream_id state ~end_stream with
            | Error error -> Error error
            | Ok next_state ->
                Cell.set stream.state next_state;
                Ok stream
          )
        | None ->
            if not (is_valid_peer_initiated_stream_id ~role:conn.role stream_id) then
              Error (InvalidPeerStreamId { role = conn.role; stream_id })
            else
              (
                match ensure_accepts_new_peer_stream conn ~stream_id with
                | Error error -> Error error
                | Ok () -> (
                    match ensure_peer_stream_id_increases conn ~stream_id with
                    | Error error -> Error error
                    | Ok () -> (
                        match ensure_stream_capacity
                          conn
                          ~initiator:PeerInitiated
                          ~stream_id
                          ~limit:(Cell.get conn.local_settings.max_concurrent_streams) with
                        | Error error -> Error error
                        | Ok () ->
                            let s = {
                              id = stream_id;
                              state =
                                Cell.create
                                  (
                                    if end_stream then
                                      StreamHalfClosedRemote
                                    else
                                      StreamOpen
                                  );
                              window_size = Cell.create
                                (Cell.get conn.remote_settings.initial_window_size);
                              receive_window_size = Cell.create
                                (Cell.get conn.local_settings.initial_window_size);
                              headers = Cell.create [];
                              data_chunks = Cell.create [];
                            }
                            in
                            Cell.set conn.peer_last_stream_id stream_id;
                            let _ = HashMap.insert conn.streams ~key:stream_id ~value:s in
                            Ok s
                      )
                  )
              )
      in
      match stream with
      | Error error -> Error error
      | Ok stream ->
          Cell.set stream.headers headers;
          Ok [ HeadersReceived { stream_id; headers; end_stream } ]

let concatenate_fragments = fun fragments -> String.concat "" fragments

let process_headers_frame = fun conn stream_id payload flags ->
  match payload with
  | Frame.HeadersPayload { header_block_fragment; _ } ->
      let end_stream = flags.Frame.end_stream in
      if flags.Frame.end_headers then
        decode_header_block conn ~stream_id ~fragment:header_block_fragment ~end_stream
      else (
        Cell.set
          conn.pending_header_block
          (Some { stream_id; fragments = [ header_block_fragment ]; end_stream });
        Ok []
      )
  | _ -> Error (InvalidPayloadForFrame { frame_type = Frame.Headers; payload })

let process_continuation_frame = fun conn stream_id payload flags ->
  match (payload, Cell.get conn.pending_header_block) with
  | (Frame.ContinuationPayload header_block_fragment, Some pending) ->
      let fragments = pending.fragments @ [ header_block_fragment ] in
      if flags.Frame.end_headers then (
        Cell.set conn.pending_header_block None;
        decode_header_block
          conn
          ~stream_id
          ~fragment:(concatenate_fragments fragments)
          ~end_stream:pending.end_stream
      ) else (
        Cell.set conn.pending_header_block (Some { pending with fragments });
        Ok []
      )
  | (Frame.ContinuationPayload _, None) -> Error (UnexpectedContinuation { stream_id })
  | _ -> Error (InvalidPayloadForFrame { frame_type = Frame.Continuation; payload })

let process_data_frame = fun conn stream_id payload flags ->
  match payload with
  | Frame.DataPayload { data; _ } ->
      let data_bytes = Bytes.from_string data in
      let end_stream = flags.Frame.end_stream in
      (
        match get_stream conn stream_id with
        | None -> Error (DataBeforeHeaders { stream_id })
        | Some stream ->
            let state = Cell.get stream.state in
            match transition_recv_data ~stream_id state ~end_stream with
            | Error error -> Error error
            | Ok next_state ->
                let data_len = Bytes.length data_bytes in
                let connection_window = Cell.get conn.receive_connection_window_size in
                let stream_window = Cell.get stream.receive_window_size in
                if data_len > connection_window then
                  Error (FlowControlWindowExceeded {
                    scope = ConnectionWindow;
                    data_size = data_len;
                    window_size = connection_window;
                  })
                else if data_len > stream_window then
                  Error (FlowControlWindowExceeded {
                    scope = StreamWindow { stream_id };
                    data_size = data_len;
                    window_size = stream_window;
                  })
                else (
                  Cell.set conn.receive_connection_window_size (connection_window - data_len);
                  Cell.set stream.receive_window_size (stream_window - data_len);
                  let chunks = Cell.get stream.data_chunks in
                  Cell.set stream.data_chunks (chunks @ [ data_bytes ]);
                  Cell.set stream.state next_state;
                  Ok [ DataReceived { stream_id; data = data_bytes; end_stream } ]
                )
      )
  | _ -> Error (InvalidPayloadForFrame { frame_type = Frame.Data; payload })

let validate_header_block_sequence = fun conn frame ->
  match (Cell.get conn.pending_header_block, frame.Frame.frame_type) with
  | (Some pending, Frame.Continuation) when frame.stream_id != pending.stream_id ->
      Error (ContinuationStreamMismatch {
        expected_stream_id = pending.stream_id;
        actual_stream_id = frame.stream_id;
      })
  | (Some _, Frame.Continuation) -> Ok ()
  | (Some pending, frame_type) ->
      Error (ExpectedContinuation { stream_id = pending.stream_id; frame_type })
  | (None, Frame.Continuation) -> Error (UnexpectedContinuation { stream_id = frame.stream_id })
  | (None, _) -> Ok ()

let process_frame_after_preface = fun conn frame ->
  match validate_header_block_sequence conn frame with
  | Error error -> Error error
  | Ok () -> (
      match frame.Frame.frame_type with
      | Frame.Settings -> (
          match frame.payload with
          | Frame.SettingsPayload settings -> process_settings_frame conn settings frame.flags
          | _ ->
              Error (InvalidPayloadForFrame { frame_type = Frame.Settings; payload = frame.payload })
        )
      | Frame.Headers -> process_headers_frame conn frame.stream_id frame.payload frame.flags
      | Frame.Data -> process_data_frame conn frame.stream_id frame.payload frame.flags
      | Frame.WindowUpdate -> (
          match frame.payload with
          | Frame.WindowUpdatePayload increment ->
              if frame.stream_id = 0 then (
                match add_flow_control_window
                  conn.send_connection_window_size
                  ~scope:ConnectionWindow
                  ~increment with
                | Error error -> Error error
                | Ok () -> Ok [ WindowUpdateReceived { stream_id = frame.stream_id; increment } ]
              ) else (
                (* Stream-level window update *)
                match get_stream conn frame.stream_id with
                | None ->
                    Error (FrameForIdleStream {
                      stream_id = frame.stream_id;
                      frame_type = Frame.WindowUpdate;
                    })
                | Some stream -> (
                    match add_flow_control_window
                      stream.window_size
                      ~scope:(StreamWindow { stream_id = frame.stream_id })
                      ~increment with
                    | Error error -> Error error
                    | Ok () ->
                        Ok [ WindowUpdateReceived { stream_id = frame.stream_id; increment }; ]
                  )
              )
          | _ ->
              Error (InvalidPayloadForFrame {
                frame_type = Frame.WindowUpdate;
                payload = frame.payload;
              })
        )
      | Frame.Ping -> (
          match frame.payload with
          | Frame.PingPayload data ->
              if frame.flags.ack then
                Ok [ PingAckReceived { data } ]
              else
                Ok [ PingReceived { data } ]
          | _ -> Error (InvalidPayloadForFrame { frame_type = Frame.Ping; payload = frame.payload })
        )
      | Frame.Goaway -> (
          match frame.payload with
          | Frame.GoawayPayload { last_stream_id; error_code; debug_data } ->
              Cell.set conn.state GoingAway;
              Ok [ GoawayReceived { last_stream_id; error_code; debug_data } ]
          | _ ->
              Error (InvalidPayloadForFrame { frame_type = Frame.Goaway; payload = frame.payload })
        )
      | Frame.RstStream -> (
          match frame.payload with
          | Frame.RstStreamPayload error_code -> (
              match get_stream conn frame.stream_id with
              | None ->
                  Error (FrameForIdleStream {
                    stream_id = frame.stream_id;
                    frame_type = Frame.RstStream;
                  })
              | Some stream ->
                  Cell.set stream.state StreamClosed;
                  Ok [ RstStreamReceived { stream_id = frame.stream_id; error_code } ]
            )
          | _ ->
              Error (InvalidPayloadForFrame {
                frame_type = Frame.RstStream;
                payload = frame.payload;
              })
        )
      | Frame.Priority -> (
          match frame.payload with
          | Frame.PriorityPayload { stream_dependency; exclusive; weight } ->
              Ok [
                PriorityReceived {
                  stream_id = frame.stream_id;
                  stream_dependency;
                  weight;
                  exclusive;
                };
              ]
          | _ ->
              Error (InvalidPayloadForFrame { frame_type = Frame.Priority; payload = frame.payload })
        )
      | Frame.PushPromise ->
          Error (UnsupportedFrameReceived {
            frame_type = Frame.PushPromise;
            payload = frame.payload;
          })
      | Frame.Continuation ->
          process_continuation_frame conn frame.stream_id frame.payload frame.flags
      | Frame.Unknown _ -> Ok []
    )

let process_frame = fun conn frame ->
  match Cell.get conn.preface_state with
  | AwaitingClientPreface _ ->
      Error (ExpectedInitialSettings { frame_type = frame.Frame.frame_type })
  | AwaitingInitialSettings ->
      if frame.Frame.frame_type != Frame.Settings then
        Error (ExpectedInitialSettings { frame_type = frame.frame_type })
      else (
        Cell.set conn.preface_state PrefaceComplete;
        process_frame_after_preface conn frame
      )
  | PrefaceComplete -> process_frame_after_preface conn frame

let parser_config = fun conn -> ({
  Parser.max_frame_size = Cell.get conn.local_settings.max_frame_size;
}: Parser.config)

let find_preface_mismatch = fun combined ->
  let available = String.length combined in
  let expected_length = String.length client_preface in
  let limit =
    if available < expected_length then
      available
    else
      expected_length
  in
  let rec loop offset =
    if offset >= limit then
      None
    else
      let expected =
        String.get_unchecked client_preface ~at:offset
        |> Char.to_int
      in
      let actual =
        String.get_unchecked combined ~at:offset
        |> Char.to_int
      in
      if expected != actual then
        Some (InvalidClientPrefaceByte { offset; expected; actual })
      else
        loop (offset + 1)
  in
  loop 0

let consume_client_preface = fun conn input ->
  match Cell.get conn.preface_state with
  | AwaitingClientPreface buffered ->
      let combined = buffered ^ input in
      (
        match find_preface_mismatch combined with
        | Some error ->
            Cell.set conn.preface_state (AwaitingClientPreface "");
            Error error
        | None ->
            let expected_length = String.length client_preface in
            let combined_length = String.length combined in
            if combined_length < expected_length then (
              Cell.set conn.preface_state (AwaitingClientPreface combined);
              Ok ""
            ) else (
              Cell.set conn.preface_state AwaitingInitialSettings;
              Ok (String.sub
                combined
                ~offset:expected_length
                ~len:(combined_length - expected_length))
            )
      )
  | AwaitingInitialSettings
  | PrefaceComplete -> Ok input

let process_data = fun conn data ->
  let rec process_all events remaining =
    match Parser.parse_frame ~config:(parser_config conn) remaining with
    | Parser.Done { value = frame; remaining = rest } -> (
        Cell.set conn.pending_input "";
        match process_frame conn frame with
        | Error e -> Error e
        | Ok new_events ->
            if String.length rest > 0 then
              process_all (events @ new_events) rest
            else
              Ok (events @ new_events)
      )
    | Parser.Need_more ->
        Cell.set conn.pending_input remaining;
        Ok events
    | Parser.Error e ->
        Cell.set conn.pending_input "";
        Error (ParserError e)
  in
  let input = Cell.get conn.pending_input ^ Bytes.to_string data in
  match consume_client_preface conn input with
  | Error error ->
      Cell.set conn.pending_input "";
      Error error
  | Ok "" -> Ok []
  | Ok input -> process_all [] input
