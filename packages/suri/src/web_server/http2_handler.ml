open Std

module Cell = Sync.Cell
module HashMap = Collections.HashMap
module Bytes = IO.Bytes

type protocol_error =
  | UpgradeNotSupported
  | UnknownDataStream of int
  | InvalidPreface
  | InvalidRequestHeaders of request_header_error

and pseudo_header =
  | Method
  | Scheme
  | Path

and request_header_error =
  | MissingPseudoHeader of pseudo_header
  | EmptyPseudoHeader of pseudo_header
  | InvalidPath of {
      value: string;
      reason: Net.Uri.error;
    }

type io_operation =
  | SendSettings
  | SendSettingsAck
  | SendHeaders
  | SendData
  | SendPing

type error =
  | ParseError of Http.Http2.Parser_reader.parse_error
  | SerializerError of Http.Http2.Serializer.error
  | FrameConstructorError of Http.Http2.Frame.constructor_error
  | HpackEncodeError of Http.Http2.Hpack.encode_error
  | HpackDecodeError of Http.Http2.Hpack.decode_error
  | ProtocolError of protocol_error
  | IoError of {
      operation: io_operation;
      error: Socket_pool.Connection.error;
    }

let parse_error_to_string = Http.Http2.Parser_reader.parse_error_to_string

let rec protocol_error_to_string = fun __tmp1 ->
  match __tmp1 with
  | UpgradeNotSupported -> "HTTP/2 upgrades are not supported"
  | UnknownDataStream stream_id -> "DATA for unknown stream " ^ Int.to_string stream_id
  | InvalidPreface -> "Invalid HTTP/2 preface"
  | InvalidRequestHeaders error ->
      "Invalid HTTP/2 request headers: " ^ request_header_error_to_string error

and pseudo_header_to_string = fun __tmp1 ->
  match __tmp1 with
  | Method -> ":method"
  | Scheme -> ":scheme"
  | Path -> ":path"

and uri_error_to_string = fun __tmp1 ->
  match __tmp1 with
  | Net.Uri.InvalidScheme -> "invalid scheme"
  | Net.Uri.InvalidAuthority -> "invalid authority"
  | Net.Uri.InvalidPath -> "invalid path"
  | Net.Uri.InvalidQuery -> "invalid query"
  | Net.Uri.InvalidFragment -> "invalid fragment"
  | Net.Uri.InvalidFormat -> "invalid format"
  | Net.Uri.TooLong -> "URI is too long"

and request_header_error_to_string = fun __tmp1 ->
  match __tmp1 with
  | MissingPseudoHeader header -> "missing required pseudo-header " ^ pseudo_header_to_string header
  | EmptyPseudoHeader header -> "empty required pseudo-header " ^ pseudo_header_to_string header
  | InvalidPath { value; reason } ->
      "invalid :path pseudo-header " ^ value ^ " (" ^ uri_error_to_string reason ^ ")"

let io_operation_to_string = fun __tmp1 ->
  match __tmp1 with
  | SendSettings -> "sending SETTINGS"
  | SendSettingsAck -> "sending SETTINGS ACK"
  | SendHeaders -> "sending HEADERS"
  | SendData -> "sending DATA"
  | SendPing -> "sending PING"

let connection_error_to_string = fun __tmp1 ->
  match __tmp1 with
  | Socket_pool.Connection.Closed -> "connection closed"
  | Socket_pool.Connection.FileError _ -> "file operation failed"
  | Socket_pool.Connection.InvalidRange { off; len; size } ->
      "invalid file range off="
      ^ Int.to_string off
      ^ ", len="
      ^ Int.to_string len
      ^ ", size="
      ^ Int.to_string size

let to_string_error = fun __tmp1 ->
  match __tmp1 with
  | ParseError err -> "HTTP/2 parse error: " ^ parse_error_to_string err
  | SerializerError err -> "HTTP/2 serializer error: " ^ Http.Http2.Serializer.error_to_string err
  | FrameConstructorError err ->
      "HTTP/2 frame constructor error: " ^ Http.Http2.Frame.constructor_error_to_string err
  | HpackEncodeError err ->
      "HTTP/2 HPACK encode error: " ^ Http.Http2.Hpack.encode_error_to_string err
  | HpackDecodeError err ->
      "HTTP/2 HPACK decode error: " ^ Http.Http2.Hpack.decode_error_to_string err
  | ProtocolError err -> "HTTP/2 protocol error: " ^ protocol_error_to_string err
  | IoError { operation; error } ->
      "HTTP/2 I/O error while "
      ^ io_operation_to_string operation
      ^ ": "
      ^ connection_error_to_string error

let io_error = fun operation error -> IoError { operation; error }

let send_frame = fun conn operation frame ->
  match Http.Http2.Serializer.serialize_frame frame with
  | Error error -> Error (SerializerError error)
  | Ok encoded -> (
      match Socket_pool.Connection.send conn encoded with
      | Ok () -> Ok ()
      | Error error -> Error (io_error operation error)
    )

(** Stream state for multiplexed requests *)
type stream_state = {
  stream_id: int;
  headers: Http.Http2.Hpack.header list Cell.t;
  data_chunks: string list Cell.t;
  end_stream: bool Cell.t;
}

type state = {
  config: Super.Config.t;
  handler: Http_handler.t;
  frame_parser: Http.Http2.Parser_reader.state;
  hpack_decoder: Http.Http2.Hpack.decoder;
  hpack_encoder: Http.Http2.Hpack.encoder;
  streams: (int, stream_state) HashMap.t;
  preface_verified: bool Cell.t;
  settings_sent: bool Cell.t;
  buffer: string Cell.t;
  (** Accumulated data buffer *)
}

let make_handler = fun ~config ~handler ?(sniffed_data = "") () ->
  {
    config;
    handler;
    frame_parser = Http.Http2.Parser_reader.create ();
    hpack_decoder = Http.Http2.Hpack.create_decoder ();
    hpack_encoder = Http.Http2.Hpack.create_encoder ();
    streams = HashMap.create ();
    preface_verified = Cell.create false;
    settings_sent = Cell.create false;
    buffer = Cell.create sniffed_data;
  }

(** Verify HTTP/2 connection preface *)
let verify_preface = fun data ->
  let preface = "PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n" in
  if String.length data >= String.length preface then
    String.sub data ~offset:0 ~len:(String.length preface) = preface
  else
    false

(** Send SETTINGS frame *)
let send_settings = fun conn ->
  let settings_frame =
    Http.Http2.Frame.settings
      [
        Http.Http2.Frame.MaxConcurrentStreams 100;
        Http.Http2.Frame.MaxFrameSize 16_384;
        Http.Http2.Frame.InitialWindowSize 65_535;
      ]
  in
  send_frame conn SendSettings settings_frame

(** Send SETTINGS ACK *)
let send_settings_ack = fun conn ->
  let ack_frame = Http.Http2.Frame.settings ~ack:true [] in
  send_frame conn SendSettingsAck ack_frame

(** Send HTTP/2 HEADERS frame *)
let send_headers = fun conn hpack_encoder stream_id headers end_stream ->
  let headers =
    List.map ~fn:(fun (name, value) -> { Http.Http2.Hpack.name = name; value }) headers
  in
  match Http.Http2.Hpack.encode hpack_encoder ~sensitive_headers:[] () ~headers with
  | Error error -> Error (HpackEncodeError error)
  | Ok encoded_headers ->
      let header_block = Bytes.to_string encoded_headers in
      let frame = Http.Http2.Frame.headers ~stream_id ~end_stream ~end_headers:true header_block in
      send_frame conn SendHeaders frame

(** Send HTTP/2 DATA frame *)
let send_data = fun conn stream_id data end_stream ->
  let frame = Http.Http2.Frame.data ~stream_id ~end_stream data in
  send_frame conn SendData frame

(** Convert HTTP/2 headers to Request.t *)
let require_pseudo_header = fun header headers ->
  let name = pseudo_header_to_string header in
  match Std.Collections.Proplist.get headers ~key:name with
  | None -> Error (MissingPseudoHeader header)
  | Some value when String.equal value "" -> Error (EmptyPseudoHeader header)
  | Some value -> Ok value

let request_of_header_pairs = fun headers body ->
  match require_pseudo_header Method headers with
  | Error error -> Error error
  | Ok method_value -> (
      match require_pseudo_header Scheme headers with
      | Error error -> Error error
      | Ok _scheme -> (
          match require_pseudo_header Path headers with
          | Error error -> Error error
          | Ok path -> (
              let method_ = Net.Http.Method.from_string method_value in
              let headers =
                List.filter ~fn:(fun (k, _) -> not (String.starts_with ~prefix:":" k)) headers
              in
              match Net.Uri.from_string path with
              | Error reason -> Error (InvalidPath { value = path; reason })
              | Ok uri ->
                  let http_request =
                    let request = Net.Http.Request.create method_ uri in
                    Net.Http.Request.with_headers request (Net.Http.Header.from_list headers)
                  in
                  Ok (Request.from_http ~body http_request)
            )
        )
    )

let headers_to_request = fun headers body ->
  headers
  |> List.map ~fn:(fun header -> (header.Http.Http2.Hpack.name, header.value))
  |> fun headers -> request_of_header_pairs headers body

(** Handle completed stream (all headers and data received *)
let handle_stream = fun conn state stream_id stream ->
  if not (Cell.get stream.end_stream) then
    Ok ()
    (* Stream not complete yet *)
  else
    let headers =
      List.rev (Cell.get stream.headers)
      |> List.map ~fn:(fun header -> (header.Http.Http2.Hpack.name, header.value))
    in
    let body = String.concat "" (List.rev (Cell.get stream.data_chunks)) in
    match request_of_header_pairs headers body with
    | Error error -> Error (ProtocolError (InvalidRequestHeaders error))
    | Ok request -> (
        (* Convert to Request and call handler *)
        match state.handler conn request with
        | Http_handler.Response response ->
            let status = response.status in
            let response_headers =
              [ (":status", Int.to_string (Net.Http.Status.to_int status)); ]
              @ Net.Http.Header.to_list response.headers
            in
            (
              match send_headers conn state.hpack_encoder stream_id response_headers false with
              | Error e -> Error e
              | Ok () ->
                  let body = response.body in
                  match send_data conn stream_id body true with
                  | Error e -> Error e
                  | Ok () ->
                      let _ = HashMap.remove state.streams ~key:stream_id in
                      Ok ()
            )
        | Http_handler.Upgrade _ -> Error (ProtocolError UpgradeNotSupported)
      )

(** Process a single HTTP/2 frame *)
let process_frame = fun conn state frame ->
  match (frame.Http.Http2.Frame.frame_type, frame.Http.Http2.Frame.payload) with
  | (Http.Http2.Frame.Settings, Http.Http2.Frame.SettingsPayload _) ->
      if not frame.Http.Http2.Frame.flags.ack then
        send_settings_ack conn
      else
        Ok ()
  | (Http.Http2.Frame.Headers, Http.Http2.Frame.HeadersPayload { header_block_fragment; _ }) ->
      let stream_id = frame.Http.Http2.Frame.stream_id in
      let end_stream = frame.Http.Http2.Frame.flags.end_stream in
      (* Decode headers *)
      (
        match Http.Http2.Hpack.decode state.hpack_decoder (Bytes.from_string header_block_fragment) with
        | Ok headers ->
            (* Get or create stream *)
            let stream =
              match HashMap.get state.streams ~key:stream_id with
              | Some s -> s
              | None ->
                  let s = {
                    stream_id;
                    headers = Cell.create [];
                    data_chunks = Cell.create [];
                    end_stream = Cell.create false;
                  }
                  in
                  let _ = HashMap.insert state.streams ~key:stream_id ~value:s in
                  s
            in
            (* Add headers *)
            Cell.set
              stream.headers
              (
                List.rev headers
                |> List.append (Cell.get stream.headers)
              );
            (* Mark if stream ended *)
            if end_stream then (
              Cell.set stream.end_stream true;
              handle_stream conn state stream_id stream
            ) else
              Ok ()
        | Error error -> Error (HpackDecodeError error)
      )
  | (Http.Http2.Frame.Data, Http.Http2.Frame.DataPayload { data; _ }) ->
      let stream_id = frame.Http.Http2.Frame.stream_id in
      let end_stream = frame.Http.Http2.Frame.flags.end_stream in
      (* Find stream *)
      (
        match HashMap.get state.streams ~key:stream_id with
        | None -> Error (ProtocolError (UnknownDataStream stream_id))
        | Some stream ->
            (* Add data *)
            Cell.set stream.data_chunks (data :: Cell.get stream.data_chunks);
            (* Mark if stream ended *)
            if end_stream then (
              Cell.set stream.end_stream true;
              handle_stream conn state stream_id stream
            ) else
              Ok ()
      )
  | (Http.Http2.Frame.Ping, Http.Http2.Frame.PingPayload opaque_data) ->
      if not frame.Http.Http2.Frame.flags.ack then
        match Http.Http2.Frame.ping ~ack:true opaque_data with
        | Error error -> Error (FrameConstructorError error)
        | Ok pong_frame -> send_frame conn SendPing pong_frame
      else
        Ok ()
  | (_, _) -> Ok ()

(** Main data handler *)
let handle_data = fun data conn state ->
  (* Accumulate data in buffer *)
  let current_buffer = Cell.get state.buffer in
  Cell.set state.buffer (current_buffer ^ data);
  let buffer_data = Cell.get state.buffer in
  (* Verify preface if not yet done *)
  if not (Cell.get state.preface_verified) then (
    if String.length buffer_data >= 24 then (
      if verify_preface buffer_data then (
        Cell.set state.preface_verified true;
        (* Remove preface from buffer *)
        Cell.set
          state.buffer
          (String.sub buffer_data ~offset:24 ~len:(String.length buffer_data - 24));
        (* Send SETTINGS *)
        match send_settings conn with
        | Error e -> Socket_pool.Handler.Error (state, e)
        | Ok () ->
            Cell.set state.settings_sent true;
            Socket_pool.Handler.Continue state
      ) else
        Socket_pool.Handler.Error (state, ProtocolError InvalidPreface)
    ) else
      (* Need more data for preface *)
      Socket_pool.Handler.Continue state
  ) else (
    (* Parse frames *)
    let reader = IO.Reader.from_string (Cell.get state.buffer) in
    let rec parse_frames () =
      match Http.Http2.Parser_reader.parse state.frame_parser reader with
      | Http.Http2.Parser_reader.Frame frame -> (
          match process_frame conn state frame with
          | Ok () -> parse_frames ()
          | Error e -> Socket_pool.Handler.Error (state, e)
        )
      | Http.Http2.Parser_reader.Need_more ->
          (* Partial frame state is stored in frame_parser; wait for more bytes *)
          Cell.set state.buffer "";
          Socket_pool.Handler.Continue state
      | Http.Http2.Parser_reader.Error err -> Socket_pool.Handler.Error (state, ParseError err)
    in
    parse_frames ()
  )

let handle_connection = fun _conn state -> Socket_pool.Handler.Continue state

let handle_error = fun _error _conn state -> Socket_pool.Handler.Close state

let handle_close = fun _conn _state -> ()

let handle_shutdown = fun _conn state -> Socket_pool.Handler.Close state

let handle_message = fun _msg _conn state -> Socket_pool.Handler.Continue state
