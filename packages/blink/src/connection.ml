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
  | ReadingChunkedBody
  | Complete

type t =
  | Conn: {
      protocol: (module Protocol.Intf);
      writer: IO.Writer.t;
      reader: IO.Reader.t;
      uri: Net.Uri.t;
      mutable buffer: Buffer.t;
      mutable state: response_state;
      mutable response: Net.Http.Response.t option;
      from_io_error: IO.error -> Error.t;
    } -> t

let make:
  reader:IO.Reader.t ->
  writer:IO.Writer.t ->
  from_io_error:(IO.error -> Error.t) ->
  uri:Net.Uri.t ->
  t = fun ~reader ~writer ~from_io_error ~uri ->
  Conn {
    protocol = (module Protocol.Http1);
    reader;
    writer;
    uri;
    buffer = Buffer.create ~size:4_096;
    state = WaitingForHeaders;
    response = None;
    from_io_error;
  }

let request = fun (Conn conn) req ?body () ->
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
  | Error e -> Error (conn.from_io_error e)

let read_more = fun (Conn conn) ->
  let chunk = IO.Buffer.create ~size:4_096 in
  match IO.read conn.reader ~into:chunk with
  | Ok 0 -> Error Error.Eof
  | Ok _ ->
      let readable = IO.Buffer.readable chunk in
      let _ =
        Buffer.append_slice conn.buffer readable
        |> Result.expect ~msg:"failed to append response chunk"
      in
      Ok ()
  | Error e -> Error (conn.from_io_error e)

let status_has_no_body = fun status ->
  let code = Net.Http.Status.to_int status in
  (code >= 100 && code < 200) || code = 204 || code = 304

let header_value_is_chunked = fun value ->
  String.equal
    (String.lowercase_ascii (String.trim value))
    "chunked"

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
            let transfer_encoding = Net.Http.Header.get headers "transfer-encoding" in
            let content_length = Net.Http.Header.get headers "content-length" in
            conn.state <- (
              if status_has_no_body status then
                Complete
              else
                match transfer_encoding with
                | Some value when header_value_is_chunked value -> ReadingChunkedBody
                | _ -> (
                    match content_length with
                    | Some len -> (
                        match Int.parse (String.trim len) with
                        | Some length -> ReadingFixedBody { length; received = 0 }
                        | None -> Complete
                      )
                    | None -> Complete
                  )
            );
            Ok [ Status status; Headers headers ]
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
  | ReadingChunkedBody ->
      let rec parse_chunks acc =
        let data = Buffer.contents conn.buffer in
        match Http.Http1.Chunk.parse data with
        | Http.Http1.Common.Done { value = { data = chunk_data; remaining }; _ } ->
            Buffer.clear conn.buffer;
            Buffer.add_string conn.buffer remaining;
            if chunk_data = "" then (
              conn.state <- Complete;
              Ok (List.reverse (Done :: acc))
            ) else
              (* Return the chunk immediately for streaming support *)
              Ok (List.reverse (Data chunk_data :: acc))
        | Http.Http1.Common.Need_more -> (
            match read_more c with
            | Ok () -> parse_chunks acc
            | Error e ->
                if not (List.is_empty acc) then
                  Ok (List.reverse acc)
                else
                  Error e
          )
        | Http.Http1.Common.Error msg -> Error (Error.ParseError msg)
      in
      parse_chunks []

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
          ~fn:(fun __tmp1 ->
            match __tmp1 with
            | Data chunk -> Some chunk
            | _ -> None)
      in
      let body = String.concat "" body_chunks in
      Ok (response, body)

let close = fun _conn -> ()
(* Reader/writer don't need explicit close *)
