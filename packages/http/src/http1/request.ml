(** HTTP/1.1 Request Parser *)
open Std

module Cursor = Std.Iter.Cursor
module Slice = IO.IoVec.IoSlice

module SliceCursor = struct
  type t = {
    source: Slice.t;
    pos: int;
    length: int;
  }

  let create = fun source -> { source; pos = 0; length = Slice.length source }

  let of_cursor = fun cursor ->
    let source = Cursor.source cursor in
    { source; pos = Cursor.position cursor; length = Slice.length source }

  let is_eof = fun cursor -> cursor.pos >= cursor.length

  let advance = fun cursor ->
    if is_eof cursor then
      None
    else
      Some { cursor with pos = cursor.pos + 1 }

  let advance_by = fun cursor count ->
    let pos = cursor.pos + count in
    if pos > cursor.length then
      None
    else
      Some { cursor with pos }

  let take_until = fun cursor predicate ->
    let start = cursor.pos in
    let rec loop pos =
      if pos >= cursor.length then
        None
      else if predicate (Slice.get_unchecked cursor.source ~at:pos) then
        Some pos
      else
        loop (pos + 1)
    in
    match loop start with
    | None -> None
    | Some stop ->
        Some (
          Slice.sub_unchecked cursor.source ~off:start ~len:(stop - start),
          { cursor with pos = stop }
        )

  let take_until_char = fun cursor needle -> take_until cursor (fun value -> value = needle)

  let take_n = fun cursor count ->
    if cursor.pos + count > cursor.length then
      None
    else
      Some (
        Slice.sub_unchecked cursor.source ~off:cursor.pos ~len:count,
        { cursor with pos = cursor.pos + count }
      )

  let take_while = fun cursor predicate ->
    let start = cursor.pos in
    let rec loop pos =
      if pos >= cursor.length then
        pos
      else if predicate (Slice.get_unchecked cursor.source ~at:pos) then
        loop (pos + 1)
      else
        pos
    in
    let stop = loop start in
    (Slice.sub_unchecked cursor.source ~off:start ~len:(stop - start), { cursor with pos = stop })

  let skip_while = fun cursor predicate ->
    let (_, cursor) = take_while cursor predicate in
    cursor

  let remaining = fun cursor ->
    if is_eof cursor then
      Slice.empty
    else
      Slice.sub_unchecked cursor.source ~off:cursor.pos ~len:(cursor.length - cursor.pos)
end

let slice_of_string = fun value ->
  match Slice.from_string value with
  | Ok slice -> slice
  | Error error -> panic ("Http1.Request.slice_of_string: " ^ Slice.error_message error)

let string_of_slice = Slice.to_string

let default_root_uri =
  Std.Net.Uri.of_string "/"
  |> Result.unwrap

let is_space = fun c -> c = ' '

let is_optional_whitespace = fun c -> c = ' ' || c = '\t'

let trim_leading_ows = fun slice ->
  SliceCursor.skip_while (SliceCursor.create slice) is_optional_whitespace
  |> SliceCursor.remaining

type request_line_owned = {
  parsed_method: Std.Net.Http.Method.t;
  parsed_uri: Std.Net.Uri.t;
  parsed_version: Std.Net.Http.Version.t;
  next_cursor: SliceCursor.t;
}

type header_line_owned = {
  header_name: string;
  header_value: string;
  next_cursor: SliceCursor.t;
}

type 'a slice_parse_result =
  | Slice_done of 'a
  | Slice_need_more
  | Slice_error of string

let skip_line_ending = fun cursor ->
  match SliceCursor.advance_by cursor 2 with
  | None -> Slice_error "Invalid line ending"
  | Some cursor -> Slice_done cursor

let take_header_block_terminator = fun cursor ->
  match SliceCursor.take_n cursor 2 with
  | Some (prefix, cursor) when Slice.equal_string prefix "\r\n" -> Some cursor
  | _ -> None

let parse_request_line_owned = fun ?(max_length = 8_192) input ->
  let cursor = SliceCursor.create input in
  match SliceCursor.take_until_char cursor '\r' with
  | None -> Slice_need_more
  | Some (line, cursor) ->
      if Slice.length line > max_length then
        Slice_error "Request line too long"
      else
        match skip_line_ending cursor with
        | Slice_need_more
        | Slice_error _ as result -> result
        | Slice_done cursor -> (
            let line_cursor = SliceCursor.create line in
            match SliceCursor.take_until_char line_cursor ' ' with
            | None -> Slice_error "Missing method"
            | Some (method_, line_cursor) ->
                let line_cursor = SliceCursor.skip_while line_cursor is_space in
                match SliceCursor.take_until_char line_cursor ' ' with
                | None -> Slice_error "Missing path"
                | Some (path, line_cursor) ->
                    let version =
                      SliceCursor.skip_while line_cursor is_space
                      |> SliceCursor.remaining
                    in
                    if not (Slice.starts_with version ~prefix:"HTTP/") then
                      Slice_error "Invalid HTTP version"
                    else
                      let method_ = Std.Net.Http.Method.from_slice method_ in
                      let uri =
                        Std.Net.Uri.from_slice path
                        |> Result.unwrap_or ~default:default_root_uri
                      in
                      let version =
                        Std.Net.Http.Version.from_slice version
                        |> Result.unwrap_or ~default:Std.Net.Http.Version.Http11
                      in
                      Slice_done {
                        parsed_method = method_;
                        parsed_uri = uri;
                        parsed_version = version;
                        next_cursor = cursor;
                      }
          )

let parse_header_line_owned = fun cursor ->
  match SliceCursor.take_until_char cursor '\r' with
  | None -> Slice_need_more
  | Some (line, cursor) -> (
      match skip_line_ending cursor with
      | Slice_need_more
      | Slice_error _ as result -> result
      | Slice_done cursor -> (
          let line_cursor = SliceCursor.create line in
          match SliceCursor.take_until_char line_cursor ':' with
          | None -> Slice_error "Invalid header format (missing colon)"
          | Some (name, line_cursor) -> (
              match SliceCursor.advance line_cursor with
              | None -> Slice_error "Invalid header format"
              | Some line_cursor ->
                  let value =
                    SliceCursor.skip_while line_cursor is_optional_whitespace
                    |> SliceCursor.remaining
                  in
                  let name = trim_leading_ows name in
                  Slice_done {
                    header_name = string_of_slice name;
                    header_value = string_of_slice value;
                    next_cursor = cursor;
                  }
            )
        )
    )

let rec parse_headers_owned = fun
  ?(max_count = 100) ?(max_length = 8_192) ?(acc = []) ?(count = 0) cursor ->
  if count >= max_count then
    Slice_error "Too many headers"
  else
    match take_header_block_terminator cursor with
    | Some cursor -> Slice_done (List.reverse acc, cursor)
    | None ->
        match parse_header_line_owned cursor with
        | Slice_need_more -> Slice_need_more
        | Slice_error error -> Slice_error error
        | Slice_done { header_name; header_value; next_cursor } ->
            if String.length header_name + String.length header_value > max_length then
              Slice_error "Header too long"
            else
              parse_headers_owned
                ~max_count
                ~max_length
                ~acc:((header_name, header_value) :: acc)
                ~count:(count + 1)
                next_cursor

let parse_headers = fun ?(max_count = 100) ?(max_length = 8_192) ?(acc = []) cursor ->
  let cursor = SliceCursor.of_cursor cursor in
  match parse_headers_owned ~max_count ~max_length ~acc ~count:(List.length acc) cursor with
  | Slice_need_more -> Common.Need_more
  | Slice_error error -> Common.Error error
  | Slice_done (headers, remaining) ->
      Common.Done {
        value = (headers, string_of_slice (SliceCursor.remaining remaining));
        remaining = "";
      }

let request_of_parts = fun method_ uri version headers_list body ->
  let headers = Std.Net.Http.Header.of_list headers_list in
  let request =
    let request = Std.Net.Http.Request.create method_ uri in
    let request = Std.Net.Http.Request.with_version request version in
    let request = Std.Net.Http.Request.with_headers request headers in
    if Slice.length body > 0 then
      Std.Net.Http.Request.with_body_slice request body
    else
      request
  in
  request

let parse_slice = fun
  ?(max_request_line = 8_192) ?(max_headers = 100) ?(max_header_length = 8_192) input ->
  match parse_request_line_owned ~max_length:max_request_line input with
  | Slice_need_more -> Common.Need_more
  | Slice_error error -> Common.Error error
  | Slice_done {
    parsed_method;
    parsed_uri;
    parsed_version;
    next_cursor
  } -> (
      match parse_headers_owned ~max_count:max_headers ~max_length:max_header_length next_cursor with
      | Slice_need_more -> Common.Need_more
      | Slice_error error -> Common.Error error
      | Slice_done (headers_list, remaining) ->
          let body = SliceCursor.remaining remaining in
          let request =
            request_of_parts parsed_method parsed_uri parsed_version headers_list body
          in
          Common.Done { value = request; remaining = "" }
    )

let parse = fun
  ?(max_request_line = 8_192) ?(max_headers = 100) ?(max_header_length = 8_192) input ->
  parse_slice
    ~max_request_line
    ~max_headers
    ~max_header_length
    (slice_of_string input)
