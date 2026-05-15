open Std

module Buffer = IO.Buffer

type message =
  | Data of string
  | Done
  | Headers of Net.Http.Header.t
  | Status of Net.Http.Status.t

type response_state =
  | WaitingForHeaders
  | ReadingFixedBody of { length: int; received: int }
  | ReadingChunkedBody of chunked_body_state
  | Complete

and chunked_body_state =
  | ReadingChunkSize
  | ReadingChunkData of { remaining: int }
  | ReadingChunkDataCrlf

type t =
  | Conn: {
      protocol: (module Protocol.Intf);
      writer: IO.Writer.t;
      reader: IO.Reader.t;
      close: unit -> unit;
      uri: Net.Uri.t;
      mutable buffer: Buffer.t;
      mutable state: response_state;
      mutable response: Net.Http.Response.t option;
      mutable closed: bool;
    } -> t

let make:
  ?on_close:(unit -> unit) ->
  reader:IO.Reader.t ->
  writer:IO.Writer.t ->
  uri:Net.Uri.t ->
  unit ->
  t = fun ?(on_close = fun () -> ()) ~reader ~writer ~uri () ->
  Conn {
    protocol = (module Protocol.Http1);
    reader;
    writer;
    close = on_close;
    uri;
    buffer = Buffer.create ~size:4_096;
    state = WaitingForHeaders;
    response = None;
    closed = false;
  }

let request = fun (Conn conn) req ?body () ->
  if conn.closed then
    Error Error.Closed
  else
    let method_ = Net.Http.Request.method_ req in
    let version = Net.Http.Request.version req in
    let headers = Net.Http.Request.headers req in
    let resource = Net.Uri.path_and_query conn.uri in
    let request_line =
      (Net.Http.Method.to_string method_)
      ^ " "
      ^ resource
      ^ " "
      ^ (Net.Http.Version.to_string version)
      ^ "\r\n"
    in
    let headers =
      (
        headers
        |> fun h ->
          Net.Http.Header.add
            h
            "host"
            (
              Net.Uri.host conn.uri
              |> Option.unwrap_or ~default:"localhost"
            )
      )
      |> fun h -> Net.Http.Header.add h "user-agent" "Riot-Blink/0.2.0"
    in
    let headers =
      match body with
      | Some b ->
          Net.Http.Header.add
            headers
            "content-length"
            (
              String.length b
              |> Int.to_string
            )
      | None -> headers
    in
    let headers_str =
      Net.Http.Header.to_list headers
      |> List.map ~fn:(fun (name, value) -> name ^ ": " ^ value ^ "\r\n")
      |> String.concat ""
    in
    let request = request_line ^ headers_str ^ "\r\n" in
    let full_request =
      match body with
      | Some b -> request ^ b
      | None -> request
    in
    let request_buffer = IO.Buffer.from_string full_request in
    match IO.write_all conn.writer ~from:request_buffer with
    | Ok () ->
        conn.state <- WaitingForHeaders;
        conn.response <- None;
        Buffer.clear conn.buffer;
        Ok ()
    | Error e -> Error (Error.from_io_error e)

let read_more = fun (Conn conn) ->
  if conn.closed then
    Error Error.Closed
  else
    let chunk = IO.Buffer.create ~size:4_096 in
    match IO.read conn.reader ~into:chunk with
    | Ok 0 -> Error Error.Eof
    | Ok _ ->
        let readable = IO.Buffer.readable chunk in
        (
          match Buffer.append_slice conn.buffer readable with
          | Ok () -> Ok ()
          | Error _ ->
              Error (Error.ProtocolError (Error.ApplicationTransportError "response buffer append failed"))
        )
    | Error e -> Error (Error.from_io_error e)

let status_has_no_body = fun status ->
  let code = Net.Http.Status.to_int status in
  (code >= 100 && code < 200) || code = 204 || code = 304

let header_value_is_chunked = fun value ->
  String.equal
    (String.lowercase_ascii (String.trim value))
    "chunked"

let transfer_encoding_is_chunked = fun values ->
  match values with
  | [ value ] -> header_value_is_chunked value
  | _ -> false

let parse_content_length_values = fun values ->
  let rec loop expected values =
    match values with
    | [] -> Ok expected
    | value :: rest -> (
        match Http.Http1.Common.parse_content_length_value value with
        | Error error -> Error (Http.Http1.Common.InvalidContentLength error)
        | Ok length -> (
            match expected with
            | None -> loop (Some length) rest
            | Some previous when previous = length -> loop expected rest
            | Some previous ->
                Error (Http.Http1.Common.ConflictingContentLength {
                  expected = previous;
                  actual = length;
                })
          )
      )
  in
  loop None values

let response_body_state = fun status headers ->
  if status_has_no_body status then
    Ok Complete
  else
    let transfer_encoding = Net.Http.Header.get_all headers "transfer-encoding" in
    let content_lengths = Net.Http.Header.get_all headers "content-length" in
    match (transfer_encoding, content_lengths) with
    | (_ :: _, _ :: _) -> Error Http.Http1.Common.TransferEncodingWithContentLength
    | (values, []) when transfer_encoding_is_chunked values ->
        Ok (ReadingChunkedBody ReadingChunkSize)
    | (_ :: _, []) -> Error Http.Http1.Common.UnsupportedTransferEncoding
    | ([], values) -> (
        match parse_content_length_values values with
        | Error error -> Error error
        | Ok None -> Ok Complete
        | Ok (Some length) -> Ok (ReadingFixedBody { length; received = 0 })
      )

let find_crlf data =
  let len = String.length data in
  let rec loop index =
    if index + 1 >= len then
      None
    else if
      String.get_unchecked data ~at:index = '\r' && String.get_unchecked data ~at:(index + 1) = '\n'
    then
      Some index
    else
      loop (index + 1)
  in
  loop 0

let chunk_size_hex_digit c =
  let code = Char.to_int c in
  if code >= Char.to_int '0' && code <= Char.to_int '9' then
    Some (code - Char.to_int '0')
  else if code >= Char.to_int 'a' && code <= Char.to_int 'f' then
    Some (code - Char.to_int 'a' + 10)
  else if code >= Char.to_int 'A' && code <= Char.to_int 'F' then
    Some (code - Char.to_int 'A' + 10)
  else
    None

let parse_chunk_size_line line =
  let size_part =
    match String.index_of line ~char:';' with
    | Some index -> String.sub line ~offset:0 ~len:index
    | None -> line
  in
  let size_part = String.trim size_part in
  let len = String.length size_part in
  if len = 0 then
    Error Error.EmptyChunkSize
  else
    let rec loop index acc =
      if index >= len then
        Ok acc
      else
        let c = String.get_unchecked size_part ~at:index in
        match chunk_size_hex_digit c with
        | None -> Error Error.InvalidChunkSize
        | Some digit ->
            if acc > (Int.max_int - digit) / 16 then
              Error Error.ChunkSizeOverflow
            else
              loop (index + 1) ((acc * 16) + digit)
    in
    loop 0 0

let replace_buffer_contents buffer data =
  Buffer.clear buffer;
  Buffer.add_string buffer data

let consume_buffer_prefix buffer count =
  let data = Buffer.contents buffer in
  let len = String.length data in
  let count = Int.min count len in
  let consumed = String.sub data ~offset:0 ~len:count in
  let remaining = String.sub data ~offset:count ~len:(len - count) in
  replace_buffer_contents buffer remaining;
  consumed

let stream = fun (Conn conn as c) ->
  match conn.state with
  | Complete -> Ok [ Done ]
  | WaitingForHeaders ->
      let rec try_parse () =
        let data = Buffer.contents conn.buffer in
        match Http.Http1.Response.parse_head data with
        | Http.Http1.Common.Done { value = response; remaining } ->
            let status = Net.Http.Response.status response in
            let headers = Net.Http.Response.headers response in
            conn.response <- Some response;
            Buffer.clear conn.buffer;
            Buffer.add_string conn.buffer remaining;
            (
              match response_body_state status headers with
              | Error error -> Error (Error.ParseError error)
              | Ok state ->
                  conn.state <- state;
                  Ok [ Status status; Headers headers ]
            )
        | Http.Http1.Common.Need_more -> (
            match read_more c with
            | Ok () -> try_parse ()
            | Error e -> Error e
          )
        | Http.Http1.Common.Error msg -> Error (Error.ParseError msg)
      in
      try_parse ()
  | ReadingFixedBody { length; received } -> (
      let data = Buffer.contents conn.buffer in
      let available = String.length data in
      let remaining = length - received in
      if remaining <= 0 then (
        (* Body complete but buffer empty *)
        conn.state <- Complete;
        Ok [ Done ]
      ) else if available >= remaining then (
        (* We have enough data in buffer to complete the body *)
        let body_data = String.sub data ~offset:0 ~len:remaining in
        let leftover = String.sub data ~offset:remaining ~len:(available - remaining) in
        Buffer.clear conn.buffer;
        Buffer.add_string conn.buffer leftover;
        conn.state <- Complete;
        Ok [ Data body_data; Done ]
      ) else if available > 0 then (
        (* Partial data available, consume it and continue *)
        Buffer.clear conn.buffer;
        conn.state <- ReadingFixedBody { length; received = received + available };
        Ok [ Data data ]
      ) else
        match read_more c with
        | Ok () -> Ok []
        | Error e -> Error e
    )
  | ReadingChunkedBody chunked_state ->
      let rec parse_chunked state =
        match state with
        | ReadingChunkSize ->
            let data = Buffer.contents conn.buffer in
            (
              match find_crlf data with
              | None -> (
                  match read_more c with
                  | Ok () -> parse_chunked ReadingChunkSize
                  | Error e -> Error e
                )
              | Some line_end -> (
                  let line = String.sub data ~offset:0 ~len:line_end in
                  match parse_chunk_size_line line with
                  | Error error -> Error (Error.ProtocolError error)
                  | Ok 0 ->
                      ignore (consume_buffer_prefix conn.buffer (line_end + 2));
                      conn.state <- Complete;
                      Ok [ Done ]
                  | Ok size ->
                      ignore (consume_buffer_prefix conn.buffer (line_end + 2));
                      parse_chunked (ReadingChunkData { remaining = size })
                )
            )
        | ReadingChunkData { remaining } ->
            let available = Buffer.readable_bytes conn.buffer in
            if available <= 0 then
              match read_more c with
              | Ok () -> parse_chunked state
              | Error e -> Error e
            else
              let count = Int.min remaining available in
              let chunk = consume_buffer_prefix conn.buffer count in
              let next_remaining = remaining - count in
              conn.state <- ReadingChunkedBody (
                if next_remaining <= 0 then
                  ReadingChunkDataCrlf
                else
                  ReadingChunkData { remaining = next_remaining }
              );
            Ok [ Data chunk ]
        | ReadingChunkDataCrlf ->
            let data = Buffer.contents conn.buffer in
            if String.length data < 2 then
              match read_more c with
              | Ok () -> parse_chunked ReadingChunkDataCrlf
              | Error e -> Error e
            else if
              String.get_unchecked data ~at:0 = '\r' && String.get_unchecked data ~at:1 = '\n'
            then (
              ignore (consume_buffer_prefix conn.buffer 2);
              parse_chunked ReadingChunkSize
            ) else
              Error (Error.ProtocolError Error.InvalidChunkDataLineEnding)
      in
      parse_chunked chunked_state

let messages = fun ?(on_message = fun _ -> ()) conn ->
  let rec loop acc =
    match stream conn with
    | Error e -> Error e
    | Ok msgs ->
        on_message msgs;
        let acc = List.append (List.reverse msgs) acc in
        if List.contains msgs ~value:Done then
          Ok (List.reverse acc)
        else
          loop acc
  in
  loop []

let await = fun ?(on_message = fun _ -> ()) (Conn conn as c) ->
  match messages ~on_message c with
  | Error e -> Error e
  | Ok msgs ->
      let response =
        conn.response
        |> Option.unwrap_or ~default:(Net.Http.Response.create (Net.Http.Status.from_int 500))
      in
      let body_chunks =
        List.filter_map
          msgs
          ~fn:(fun message ->
            match message with
            | Data chunk -> Some chunk
            | _ -> None)
      in
      let body = String.concat "" body_chunks in
      Ok (response, body)

let close = fun (Conn conn) ->
  if not conn.closed then (
    conn.closed <- true;
    conn.close ()
  )
