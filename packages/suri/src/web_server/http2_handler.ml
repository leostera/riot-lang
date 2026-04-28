open Std

module Cell = Sync.Cell
module HashMap = Collections.HashMap
module Bytes = IO.Bytes

type error = [
  | `Parse_error of Http.Http2.Parser_reader.parse_error
  | `Protocol_error of string
  | `Io_error of string
]

let parse_error_to_string = function
  | Http.Http2.Parser_reader.Incomplete_frame_header -> "incomplete frame header"
  | Http.Http2.Parser_reader.Frame_size_exceeds_maximum { size; max_size } ->
      "frame size " ^ Int.to_string size ^ " exceeds maximum " ^ Int.to_string max_size
  | Http.Http2.Parser_reader.Unknown_frame_type frame_type ->
      "unknown frame type " ^ Int.to_string frame_type
  | Http.Http2.Parser_reader.Invalid_payload_length { frame_type; expected; actual } ->
      "invalid "
      ^ frame_type
      ^ " payload length: expected "
      ^ Int.to_string expected
      ^ ", got "
      ^ Int.to_string actual
  | Http.Http2.Parser_reader.Incomplete_settings_payload -> "incomplete settings payload"

let to_string_error = function
  | `Parse_error err -> "HTTP/2 parse error: " ^ parse_error_to_string err
  | `Protocol_error msg -> "HTTP/2 protocol error: " ^ msg
  | `Io_error msg -> "IO error: " ^ msg

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
  let encoded = Http.Http2.Serializer.serialize_frame settings_frame in
  match Socket_pool.Connection.send conn encoded with
  | Ok () -> Ok ()
  | Error `Closed -> Error (`Io_error "Connection closed while sending SETTINGS")

(** Send SETTINGS ACK *)
let send_settings_ack = fun conn ->
  let ack_frame = Http.Http2.Frame.settings ~ack:true [] in
  let encoded = Http.Http2.Serializer.serialize_frame ack_frame in
  match Socket_pool.Connection.send conn encoded with
  | Ok () -> Ok ()
  | Error `Closed -> Error (`Io_error "Connection closed while sending SETTINGS ACK")

(** Send HTTP/2 HEADERS frame *)
let send_headers = fun conn hpack_encoder stream_id headers end_stream ->
  let headers =
    List.map ~fn:(fun (name, value) -> { Http.Http2.Hpack.name = name; value }) headers
  in
  let header_block =
    Http.Http2.Hpack.encode hpack_encoder ~sensitive_headers:[] () ~headers
    |> Bytes.to_string
  in
  let frame = Http.Http2.Frame.headers ~stream_id ~end_stream ~end_headers:true header_block in
  let encoded = Http.Http2.Serializer.serialize_frame frame in
  match Socket_pool.Connection.send conn encoded with
  | Ok () -> Ok ()
  | Error `Closed -> Error (`Io_error "Connection closed while sending HEADERS")

(** Send HTTP/2 DATA frame *)
let send_data = fun conn stream_id data end_stream ->
  let frame = Http.Http2.Frame.data ~stream_id ~end_stream data in
  let encoded = Http.Http2.Serializer.serialize_frame frame in
  match Socket_pool.Connection.send conn encoded with
  | Ok () -> Ok ()
  | Error `Closed -> Error (`Io_error "Connection closed while sending DATA")

(** Convert HTTP/2 headers to Request.t *)
let headers_to_request = fun conn headers body ->
  let method_ =
    Std.Collections.Proplist.get headers ~key:":method"
    |> Option.map ~fn:Net.Http.Method.of_string
    |> Option.unwrap_or ~default:Net.Http.Method.Get
  in
  let uri =
    Std.Collections.Proplist.get headers ~key:":path"
    |> Option.unwrap_or ~default:"/"
  in
  let headers = List.filter ~fn:(fun (k, _) -> not (String.starts_with ~prefix:":" k)) headers in
  let uri =
    Net.Uri.of_string uri
    |> Result.expect ~msg:"HTTP/2 request path must be a valid URI"
  in
  let http_request =
    let request = Net.Http.Request.create method_ uri in
    Net.Http.Request.with_headers request (Net.Http.Header.of_list headers)
  in
  let _ = conn in
  Request.of_http ~body http_request

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
    (* Convert to Request and call handler *)
    let request = headers_to_request conn headers body in
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
    | Http_handler.Upgrade _ -> Error (`Protocol_error "HTTP/2 upgrades are not supported")

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
        | Error _ -> Error (`Protocol_error "HPACK decode error")
      )
  | (Http.Http2.Frame.Data, Http.Http2.Frame.DataPayload { data; _ }) ->
      let stream_id = frame.Http.Http2.Frame.stream_id in
      let end_stream = frame.Http.Http2.Frame.flags.end_stream in
      (* Find stream *)
      (
        match HashMap.get state.streams ~key:stream_id with
        | None -> Error (`Protocol_error ("DATA for unknown stream " ^ Int.to_string stream_id))
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
        let pong_frame = Http.Http2.Frame.ping ~ack:true opaque_data in
        let encoded = Http.Http2.Serializer.serialize_frame pong_frame in
        (
          match Socket_pool.Connection.send conn encoded with
          | Ok () -> Ok ()
          | Error `Closed -> Error (`Io_error "Connection closed while sending PING")
        )
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
  if not (Cell.get state.preface_verified) then
    begin
      if String.length buffer_data >= 24 then
        begin
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
            Socket_pool.Handler.Error (state, `Protocol_error "Invalid HTTP/2 preface")
        end
      else
        (* Need more data for preface *)
        Socket_pool.Handler.Continue state
    end
  else
    begin
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
        | Http.Http2.Parser_reader.Error err -> Socket_pool.Handler.Error (state, `Parse_error err)
      in
      parse_frames ()
    end

let handle_connection = fun _conn state -> Socket_pool.Handler.Continue state

let handle_error = fun _error _conn state -> Socket_pool.Handler.Close state

let handle_close = fun _conn _state -> ()

let handle_shutdown = fun _conn state -> Socket_pool.Handler.Close state

let handle_message = fun _msg _conn state -> Socket_pool.Handler.Continue state
