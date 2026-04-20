(** HTTP/1.1 Request Parser *)
open Std
module Cursor = Std.Iter.Cursor
module Slice = IO.IoVec.IoSlice

let slice_of_string = fun value ->
  match Slice.from_string value with
  | Ok slice -> slice
  | Error error -> panic ("Http1.Request.slice_of_string: " ^ Kernel.IO.Error.message error)

let string_of_slice = Slice.to_string

let default_root_uri = Std.Net.Uri.of_string "/" |> Result.unwrap

let is_space = fun c -> c = ' '

let is_optional_whitespace = fun c -> c = ' ' || c = '\t'

let trim_leading_ows = fun slice ->
  Cursor.skip_while (Cursor.from_slice slice) is_optional_whitespace
  |> Cursor.remaining

type request_line_slices = {
  method_: Slice.t;
  path: Slice.t;
  version: Slice.t;
  remaining: Cursor.t;
}

type header_line_slice = {
  name: Slice.t;
  value: Slice.t;
  remaining: Cursor.t;
}

type request_line_owned = {
  parsed_method: Std.Net.Http.Method.t;
  parsed_uri: Std.Net.Uri.t;
  parsed_version: Std.Net.Http.Version.t;
  next_cursor: Cursor.t;
}

type header_line_owned = {
  header_name: string;
  header_value: string;
  next_cursor: Cursor.t;
}

type 'a slice_parse_result =
  | Slice_done of 'a
  | Slice_need_more
  | Slice_error of string

let skip_line_ending = fun cursor ->
  match Cursor.advance_by cursor 2 with
  | None -> Slice_error "Invalid line ending"
  | Some cursor -> Slice_done cursor

let take_header_block_terminator = fun cursor ->
  match Cursor.take_n cursor 2 with
  | Some (prefix, cursor) when Slice.equal_string prefix "\r\n" -> Some cursor
  | _ -> None

let parse_request_line_slice = fun ?(max_length = 8_192) input ->
  let cursor = Cursor.from_slice input in
  match Cursor.take_until_char cursor '\r' with
  | None -> Slice_need_more
  | Some (line, cursor) ->
      if Slice.length line > max_length then
        Slice_error "Request line too long"
      else
        match skip_line_ending cursor with
        | Slice_need_more | Slice_error _ as result -> result
        | Slice_done cursor -> (
            let line_cursor = Cursor.from_slice line in
            match Cursor.take_until_char line_cursor ' ' with
            | None -> Slice_error "Missing method"
            | Some (method_, line_cursor) ->
                let line_cursor = Cursor.skip_while line_cursor is_space in
                match Cursor.take_until_char line_cursor ' ' with
                | None -> Slice_error "Missing path"
                | Some (path, line_cursor) ->
                    let version = Cursor.skip_while line_cursor is_space |> Cursor.remaining in
                    if Slice.starts_with version ~prefix:"HTTP/" then
                      Slice_done { method_; path; version; remaining = cursor }
                    else
                      Slice_error "Invalid HTTP version"
          )

let parse_header_line_slice = fun cursor ->
  match Cursor.take_until_char cursor '\r' with
  | None -> Slice_need_more
  | Some (line, cursor) -> (
      match skip_line_ending cursor with
      | Slice_need_more | Slice_error _ as result -> result
      | Slice_done cursor -> (
          let line_cursor = Cursor.from_slice line in
          match Cursor.take_until_char line_cursor ':' with
          | None -> Slice_error "Invalid header format (missing colon)"
          | Some (name, line_cursor) -> (
              match Cursor.advance line_cursor with
              | None -> Slice_error "Invalid header format"
              | Some line_cursor ->
                  let value =
                    Cursor.skip_while line_cursor is_optional_whitespace
                    |> Cursor.remaining
                  in
                  let name = trim_leading_ows name in
                  Slice_done ({ name; value; remaining = cursor }: header_line_slice)
            )
        )
    )

let parse_request_line_owned = fun ?(max_length = 8_192) input ->
  match parse_request_line_slice ~max_length input with
  | Slice_need_more ->
      Slice_need_more
  | Slice_error error ->
      Slice_error error
  | Slice_done { method_; path; version; remaining } ->
      let method_ = Std.Net.Http.Method.from_slice method_ in
      let uri = Std.Net.Uri.from_slice path |> Result.unwrap_or ~default:default_root_uri in
      let version = Std.Net.Http.Version.from_slice version
      |> Result.unwrap_or ~default:Std.Net.Http.Version.Http11 in
      Slice_done {
        parsed_method = method_;
        parsed_uri = uri;
        parsed_version = version;
        next_cursor = remaining
      }

let parse_header_line_owned = fun cursor ->
  match parse_header_line_slice cursor with
  | Slice_need_more -> Slice_need_more
  | Slice_error error -> Slice_error error
  | Slice_done { name; value; remaining } -> Slice_done {
    header_name = string_of_slice name;
    header_value = string_of_slice value;
    next_cursor = remaining
  }

let rec parse_headers_owned = fun ?(max_count = 100) ?(max_length = 8_192) ?(acc = []) ?(count = 0) cursor ->
  if count >= max_count then
    Slice_error "Too many headers"
  else
    match take_header_block_terminator cursor with
    | Some cursor ->
        Slice_done (List.reverse acc, cursor)
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
  match parse_headers_owned ~max_count ~max_length ~acc ~count:(List.length acc) cursor with
  | Slice_need_more -> Common.Need_more
  | Slice_error error -> Common.Error error
  | Slice_done (headers, remaining) -> Common.Done {
    value = (headers, string_of_slice (Cursor.remaining remaining));
    remaining = ""
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

let parse_slice = fun ?(max_request_line = 8_192) ?(max_headers = 100) ?(max_header_length = 8_192) input ->
  match parse_request_line_owned ~max_length:max_request_line input with
  | Slice_need_more ->
      Common.Need_more
  | Slice_error error ->
      Common.Error error
  | Slice_done { parsed_method; parsed_uri; parsed_version; next_cursor } -> (
      match parse_headers_owned ~max_count:max_headers ~max_length:max_header_length next_cursor with
      | Slice_need_more ->
          Common.Need_more
      | Slice_error error ->
          Common.Error error
      | Slice_done (headers_list, remaining) ->
          let body = Cursor.remaining remaining in
          let request = request_of_parts parsed_method parsed_uri parsed_version headers_list body in
          Common.Done { value = request; remaining = "" }
    )

let parse = fun ?(max_request_line = 8_192) ?(max_headers = 100) ?(max_header_length = 8_192) input ->
  parse_slice ~max_request_line ~max_headers ~max_header_length (slice_of_string input)
