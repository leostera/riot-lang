(** HTTP/1.1 Response Parser *)
open Std
open Std.Iter
open Common

module Slice = IO.IoVec.IoSlice

type t = Std.Net.Http.Response.t

type status_line_slices = {
  version: Slice.t;
  status_code: int;
  reason: Slice.t;
  remaining: Cursor.t;
}

type 'a cursor_parse_result =
  | Cursor_done of {
      value: 'a;
      remaining: Cursor.t;
    }
  | Cursor_need_more
  | Cursor_error of Common.error

let slice_of_string = fun value ->
  match Slice.from_string value with
  | Ok slice -> slice
  | Error error -> panic ("Http1.Response.slice_of_string: " ^ Slice.error_message error)

let parse_status_line_slice = fun input ->
  let cursor = Cursor.from_slice input in
  match Cursor.take_until_char cursor '\r' with
  | None -> Cursor_need_more
  | Some (line, cursor) -> (
      match Cursor.advance_by cursor 2 with
      | None -> Cursor_error Common.InvalidCrlf
      | Some cursor -> (
          let line_cursor = Cursor.from_slice line in
          match Cursor.take_until_char line_cursor ' ' with
          | None -> Cursor_error Common.MissingVersion
          | Some (version, line_cursor) -> (
              let line_cursor = Cursor.skip_while line_cursor (fun c -> c = ' ') in
              match Cursor.take_until_char line_cursor ' ' with
              | None -> Cursor_error Common.MissingStatusCode
              | Some (code_str, line_cursor) -> (
                  let line_cursor = Cursor.skip_while line_cursor (fun c -> c = ' ') in
                  let reason = Cursor.remaining line_cursor in
                  let version_cursor = Cursor.from_slice version in
                  match Cursor.take_n version_cursor 5 with
                  | Some (prefix, _) when Slice.equal_string prefix "HTTP/" -> (
                      match Int.parse (Slice.to_string code_str) with
                      | None -> Cursor_error Common.InvalidStatusCode
                      | Some status_code ->
                          Cursor_done {
                            value =
                              {
                                version;
                                status_code;
                                reason;
                                remaining = cursor;
                              };
                            remaining = cursor;
                          }
                    )
                  | _ -> Cursor_error Common.InvalidHttpVersion
                )
            )
        )
    )

let parse_slice = fun input ->
  match parse_status_line_slice input with
  | Cursor_need_more -> Need_more
  | Cursor_error error -> Error error
  | Cursor_done { value = {
    version;
    status_code;
    reason;
    remaining
  }; _ } -> (
      match Request.parse_headers remaining with
      | Need_more -> Need_more
      | Error error -> Error error
      | Done { value = (headers_list, body_start); _ } -> (
          match Std.Net.Http.Version.from_slice version with
          | Error _ -> Error Common.InvalidHttpVersion
          | Ok version ->
              let status = Std.Net.Http.Status.of_int status_code in
              let headers = Std.Net.Http.Header.of_list headers_list in
              let response =
                (
                  Std.Net.Http.Response.create status
                  |> fun res ->
                    Std.Net.Http.Response.with_version res version
                    |> fun res -> Std.Net.Http.Response.with_headers res headers
                )
                |> fun res ->
                  if String.length body_start > 0 then
                    Std.Net.Http.Response.with_body res body_start
                  else
                    res
              in
              let _ = reason in
              Done { value = response; remaining = body_start }
        )
    )

let parse = fun input -> parse_slice (slice_of_string input)
