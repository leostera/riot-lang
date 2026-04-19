(** HTTP/1.1 Request Parser *)
open Std

module Cursor = Std.Iter.Cursor
module Slice = IO.Iovec.IoSlice

let slice_of_string = fun value ->
  match Slice.from_string value with
  | Ok slice -> slice
  | Error error -> panic ("Http1.Request.slice_of_string: " ^ Kernel.IO.Error.message error)

let string_of_slice = Slice.to_string

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

type 'a slice_parse_result =
  | Slice_done of 'a
  | Slice_need_more
  | Slice_error of string

let header_pair_of_slices = fun (name, value) -> (string_of_slice name, string_of_slice value)

let slice_header_pair = fun (name, value) -> (slice_of_string name, slice_of_string value)

let parse_request_line_slice = fun ?(max_length = 8_192) input ->
  let cursor = Cursor.from_slice input in
  match Cursor.take_until cursor (fun c -> c = '\r') with
  | None ->
      Slice_need_more
  | Some (line, cursor) ->
      if Slice.length line > max_length then
        Slice_error "Request line too long"
      else
        match Cursor.advance_by cursor 2 with
        | None ->
            Slice_error "Invalid line ending"
        | Some cursor -> (
            let line_cursor = Cursor.from_slice line in
            match Cursor.take_until line_cursor (fun c -> c = ' ') with
            | None ->
                Slice_error "Missing method"
            | Some (method_, line_cursor) ->
                let line_cursor = Cursor.skip_while line_cursor (fun c -> c = ' ') in
                match Cursor.take_until line_cursor (fun c -> c = ' ') with
                | None ->
                    Slice_error "Missing path"
                | Some (path, line_cursor) ->
                    let version = Cursor.skip_while line_cursor (fun c -> c = ' ') |> Cursor.remaining in
                    if Slice.starts_with version ~prefix:"HTTP/" then
                      Slice_done { method_; path; version; remaining = cursor }
                    else
                      Slice_error "Invalid HTTP version"
          )

let parse_header_line_slice = fun cursor ->
  match Cursor.take_until cursor (fun c -> c = '\r') with
  | None ->
      Slice_need_more
  | Some (line, cursor) -> (
      match Cursor.advance_by cursor 2 with
      | None ->
          Slice_error "Invalid line ending"
      | Some cursor -> (
          let line_cursor = Cursor.from_slice line in
          match Cursor.take_until line_cursor (fun c -> c = ':') with
          | None ->
              Slice_error "Invalid header format (missing colon)"
          | Some (name, line_cursor) -> (
              match Cursor.advance line_cursor with
              | None ->
                  Slice_error "Invalid header format"
              | Some line_cursor ->
                  let value =
                    Cursor.skip_while line_cursor (fun c -> c = ' ' || c = '\t')
                    |> Cursor.remaining
                  in
                  let name =
                    Cursor.skip_while (Cursor.from_slice name) (fun c -> c = ' ' || c = '\t')
                    |> Cursor.remaining
                  in
                  Slice_done { name; value; remaining = cursor }
            )
        )
    )

let rec parse_headers_slices = fun ?(max_count = 100) ?(max_length = 8_192) ?(acc = []) ?(count = 0) cursor ->
  if count >= max_count then
    Slice_error "Too many headers"
  else
    let remaining = Cursor.remaining cursor in
    if Slice.starts_with remaining ~prefix:"\r\n" then
      match Cursor.advance_by cursor 2 with
      | None ->
          Slice_need_more
      | Some cursor ->
          Slice_done (List.reverse acc, cursor)
    else
      match parse_header_line_slice cursor with
      | Slice_need_more ->
          Slice_need_more
      | Slice_error error ->
          Slice_error error
      | Slice_done { name; value; remaining } ->
          if Slice.length name + Slice.length value > max_length then
            Slice_error "Header too long"
          else
            parse_headers_slices
              ~max_count
              ~max_length
              ~acc:((name, value) :: acc)
              ~count:(count + 1)
              remaining

let parse_headers = fun ?(max_count = 100) ?(max_length = 8_192) ?(acc = []) cursor ->
  match
    parse_headers_slices
      ~max_count
      ~max_length
      ~acc:(List.map acc ~fn:slice_header_pair)
      ~count:(List.length acc)
      cursor
  with
  | Slice_need_more ->
      Common.Need_more
  | Slice_error error ->
      Common.Error error
  | Slice_done (headers, remaining) ->
      Common.Done {
        value = (List.map headers ~fn:header_pair_of_slices, string_of_slice (Cursor.remaining remaining));
        remaining = "";
      }

let parse_slice = fun ?(max_request_line = 8_192) ?(max_headers = 100) ?(max_header_length = 8_192) input ->
  match parse_request_line_slice ~max_length:max_request_line input with
  | Slice_need_more ->
      Common.Need_more
  | Slice_error error ->
      Common.Error error
  | Slice_done { method_; path; version; remaining } -> (
      match parse_headers_slices ~max_count:max_headers ~max_length:max_header_length remaining with
      | Slice_need_more ->
          Common.Need_more
      | Slice_error error ->
          Common.Error error
      | Slice_done (headers_list, remaining) ->
          let body_start = Cursor.remaining remaining in
          let method_ = string_of_slice method_ |> Std.Net.Http.Method.of_string in
          let uri =
            string_of_slice path
            |> Std.Net.Uri.of_string
            |> Result.unwrap_or ~default:(Std.Net.Uri.of_string "/" |> Result.unwrap)
          in
          let version =
            string_of_slice version
            |> Std.Net.Http.Version.of_string
            |> Result.unwrap_or ~default:Std.Net.Http.Version.Http11
          in
          let headers = List.map headers_list ~fn:header_pair_of_slices |> Std.Net.Http.Header.of_list in
          let body = string_of_slice body_start in
          let request =
            let request = Std.Net.Http.Request.create method_ uri in
            let request = Std.Net.Http.Request.with_version request version in
            let request = Std.Net.Http.Request.with_headers request headers in
            if Slice.length body_start > 0 then
              Std.Net.Http.Request.with_body request body
            else
              request
          in
          Common.Done { value = request; remaining = body }
    )

let parse = fun ?(max_request_line = 8_192) ?(max_headers = 100) ?(max_header_length = 8_192) input ->
  parse_slice ~max_request_line ~max_headers ~max_header_length (slice_of_string input)
