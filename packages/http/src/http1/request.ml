(** HTTP/1.1 Request Parser *)
open Std

module View = IO.StringView

type request_line_views = {
  method_: View.t;
  path: View.t;
  version: View.t;
  remaining: View.t;
}

type header_line_view = {
  name: View.t;
  value: View.t;
  remaining: View.t;
}

type 'a view_parse_result =
  | View_done of 'a
  | View_need_more
  | View_error of string

module View_cursor = struct
  type t = View.t

  let length_remaining = View.length

  let remaining = fun cursor -> cursor

  let advance = fun cursor ->
    if View.length cursor < 1 then
      None
    else
      Some (View.advance cursor ~by:1)

  let advance_by = fun cursor count ->
    if count < 0 || View.length cursor < count then
      None
    else
      Some (View.advance cursor ~by:count)

  let take_n = fun cursor count ->
    if count < 0 || View.length cursor < count then
      None
    else
      Some (View.sub cursor ~offset:0 ~len:count, View.advance cursor ~by:count)

  let take_until = fun cursor predicate ->
    let rec loop index =
      if index >= View.length cursor then
        None
      else if predicate (View.get cursor ~at:index) then
        Some (View.sub cursor ~offset:0 ~len:index, View.advance cursor ~by:index)
      else
        loop (index + 1)
    in
    loop 0

  let skip_while = fun cursor predicate ->
    let rec loop index =
      if index >= View.length cursor then
        cursor
      else if predicate (View.get cursor ~at:index) then
        loop (index + 1)
      else
        View.advance cursor ~by:index
    in
    loop 0
end

let string_of_view = fun view -> View.to_string view

let header_pair_of_views = fun (name, value) -> (string_of_view name, string_of_view value)

let view_header_pair = fun (name, value) -> (View.of_string name, View.of_string value)

let parse_request_line_view = fun ?(max_length = 8_192) input ->
  match View_cursor.take_until input (fun c -> c = '\r') with
  | None ->
      View_need_more
  | Some (line, cursor) ->
      if View_cursor.length_remaining line > max_length then
        View_error "Request line too long"
      else
        match View_cursor.advance_by cursor 2 with
        | None ->
            View_error "Invalid line ending"
        | Some cursor -> (
            match View_cursor.take_until line (fun c -> c = ' ') with
            | None ->
                View_error "Missing method"
            | Some (method_, line_cursor) ->
                let line_cursor = View_cursor.skip_while line_cursor (fun c -> c = ' ') in
                match View_cursor.take_until line_cursor (fun c -> c = ' ') with
                | None ->
                    View_error "Missing path"
                | Some (path, line_cursor) ->
                    let version = View_cursor.skip_while line_cursor (fun c -> c = ' ') |> View_cursor.remaining in
                    if View.starts_with version ~prefix:"HTTP/" then
                      View_done { method_; path; version; remaining = cursor }
                    else
                      View_error "Invalid HTTP version"
          )

let parse_header_line_view = fun cursor ->
  match View_cursor.take_until cursor (fun c -> c = '\r') with
  | None ->
      View_need_more
  | Some (line, cursor) -> (
      match View_cursor.advance_by cursor 2 with
      | None ->
          View_error "Invalid line ending"
      | Some cursor -> (
          match View_cursor.take_until line (fun c -> c = ':') with
          | None ->
              View_error "Invalid header format (missing colon)"
          | Some (name, line_cursor) -> (
              match View_cursor.advance line_cursor with
              | None ->
                  View_error "Invalid header format"
              | Some line_cursor ->
                  let value =
                    View_cursor.skip_while line_cursor (fun c -> c = ' ' || c = '\t')
                    |> View_cursor.remaining
                  in
                  let name =
                    View_cursor.skip_while name (fun c -> c = ' ' || c = '\t')
                    |> View_cursor.remaining
                  in
                  View_done { name; value; remaining = cursor }
            )
        )
    )

let rec parse_headers_views = fun ?(max_count = 100) ?(max_length = 8_192) ?(acc = []) ?(count = 0) cursor ->
  if count >= max_count then
    View_error "Too many headers"
  else if View.starts_with cursor ~prefix:"\r\n" then
    match View_cursor.advance_by cursor 2 with
    | None ->
        View_need_more
    | Some cursor ->
        View_done (List.reverse acc, cursor)
  else
    match parse_header_line_view cursor with
    | View_need_more ->
        View_need_more
    | View_error error ->
        View_error error
    | View_done { name; value; remaining } ->
        if View.length name + View.length value > max_length then
          View_error "Header too long"
        else
          parse_headers_views
            ~max_count
            ~max_length
            ~acc:((name, value) :: acc)
            ~count:(count + 1)
            remaining

let parse_headers = fun ?(max_count = 100) ?(max_length = 8_192) ?(acc = []) cursor ->
  let input = Std.Iter.Cursor.remaining cursor in
  match
    parse_headers_views
      ~max_count
      ~max_length
      ~acc:(List.map acc ~fn:view_header_pair)
      ~count:(List.length acc)
      (View.of_string input)
  with
  | View_need_more ->
      Common.Need_more
  | View_error error ->
      Common.Error error
  | View_done (headers, remaining) ->
      Common.Done { value = (List.map headers ~fn:header_pair_of_views, string_of_view remaining); remaining = "" }

let parse_string_view = fun ?(max_request_line = 8_192) ?(max_headers = 100) ?(max_header_length = 8_192) input ->
  match parse_request_line_view ~max_length:max_request_line input with
  | View_need_more ->
      Common.Need_more
  | View_error error ->
      Common.Error error
  | View_done { method_; path; version; remaining } -> (
      match parse_headers_views ~max_count:max_headers ~max_length:max_header_length remaining with
      | View_need_more ->
          Common.Need_more
      | View_error error ->
          Common.Error error
      | View_done (headers_list, body_start) ->
          let method_ = string_of_view method_ |> Std.Net.Http.Method.of_string in
          let uri =
            string_of_view path
            |> Std.Net.Uri.of_string
            |> Result.unwrap_or ~default:(Std.Net.Uri.of_string "/" |> Result.unwrap)
          in
          let version =
            string_of_view version
            |> Std.Net.Http.Version.of_string
            |> Result.unwrap_or ~default:Std.Net.Http.Version.Http11
          in
          let headers = List.map headers_list ~fn:header_pair_of_views |> Std.Net.Http.Header.of_list in
          let body = string_of_view body_start in
          let request =
            let request = Std.Net.Http.Request.create method_ uri in
            let request = Std.Net.Http.Request.with_version request version in
            let request = Std.Net.Http.Request.with_headers request headers in
            if View.length body_start > 0 then
              Std.Net.Http.Request.with_body request body
            else
              request
          in
          Common.Done { value = request; remaining = body }
    )

let parse = fun ?(max_request_line = 8_192) ?(max_headers = 100) ?(max_header_length = 8_192) input ->
  parse_string_view ~max_request_line ~max_headers ~max_header_length (View.of_string input)
