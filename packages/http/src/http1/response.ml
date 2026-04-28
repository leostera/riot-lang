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

type 'a body_parse_result =
  | Body_done of 'a
  | Body_need_more
  | Body_error of Common.error

type body_framing =
  | CloseDelimitedBody
  | FixedBody of int
  | ChunkedBody

let slice_of_string = fun value ->
  match Slice.from_string value with
  | Ok slice -> slice
  | Error error -> panic ("Http1.Response.slice_of_string: " ^ Slice.error_message error)

let parse_status_line_slice = fun input ->
  let cursor = Cursor.from_slice input in
  match Cursor.take_until_char cursor '\r' with
  | None -> Cursor_need_more
  | Some (line, cursor) -> (
      match Cursor.take_n cursor 2 with
      | None -> Cursor_need_more
      | Some (ending, cursor) when Slice.equal_string ending "\r\n" -> (
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
      | Some _ -> Cursor_error Common.InvalidCrlf
    )

let header_name_equal = fun left right ->
  match String.compare (String.lowercase_ascii left) (String.lowercase_ascii right) with
  | Order.EQ -> true
  | Order.LT
  | Order.GT -> false

let header_values = fun headers name ->
  let rec loop acc = function
    | [] -> List.reverse acc
    | (header_name, value) :: rest ->
        if header_name_equal header_name name then
          loop (value :: acc) rest
        else
          loop acc rest
  in
  loop [] headers

let parse_content_length_value = fun value ->
  match Int.parse (String.trim value) with
  | None -> Body_error Common.InvalidContentLength
  | Some length when length < 0 -> Body_error Common.InvalidContentLength
  | Some length -> Body_done length

let parse_content_length_values = fun values ->
  let rec loop expected = function
    | [] ->
        Body_done expected
    | value :: rest -> (
        match parse_content_length_value value with
        | Body_need_more -> Body_need_more
        | Body_error error -> Body_error error
        | Body_done length -> (
            match expected with
            | None -> loop (Some length) rest
            | Some previous when previous = length -> loop expected rest
            | Some _ -> Body_error Common.ConflictingContentLength
          )
      )
  in
  loop None values

let transfer_encoding_is_chunked = fun values ->
  match values with
  | [ value ] -> (
      match String.compare (String.lowercase_ascii (String.trim value)) "chunked" with
      | Order.EQ -> true
      | Order.LT
      | Order.GT -> false
    )
  | _ -> false

let parse_body_framing = fun headers ->
  let transfer_encoding = header_values headers "Transfer-Encoding" in
  let content_lengths = header_values headers "Content-Length" in
  match (transfer_encoding, content_lengths) with
  | (_ :: _, _ :: _) -> Body_error Common.TransferEncodingWithContentLength
  | (values, []) when transfer_encoding_is_chunked values -> Body_done ChunkedBody
  | (_ :: _, []) -> Body_error Common.UnsupportedTransferEncoding
  | ([], values) -> (
      match parse_content_length_values values with
      | Body_need_more -> Body_need_more
      | Body_error error -> Body_error error
      | Body_done None -> Body_done CloseDelimitedBody
      | Body_done (Some length) -> Body_done (FixedBody length)
    )

let split_fixed_body = fun input length ->
  let available = String.length input in
  if available < length then
    Body_need_more
  else
    let body = String.sub input ~offset:0 ~len:length in
    let remaining = String.sub input ~offset:length ~len:(available - length) in
    Body_done (body, remaining)

let status_has_no_body = fun status_code ->
  (status_code >= 100 && status_code < 200) || status_code = 204 || status_code = 304

let response_of_parts = fun status_code version headers_list body ->
  let status = Std.Net.Http.Status.of_int status_code in
  let headers = Std.Net.Http.Header.of_list headers_list in
  let response =
    Std.Net.Http.Response.create status
    |> fun res ->
      Std.Net.Http.Response.with_version res version
      |> fun res -> Std.Net.Http.Response.with_headers res headers
  in
  if String.length body > 0 then
    Std.Net.Http.Response.with_body response body
  else
    response

let parse_slice = fun input ->
  match parse_status_line_slice input with
  | Cursor_need_more -> Need_more
  | Cursor_error error -> Error error
  | Cursor_done { value = {
    version;
    status_code;
    reason = _;
    remaining
  }; _ } -> (
      match Request.parse_headers remaining with
      | Need_more -> Need_more
      | Error error -> Error error
      | Done { value = (headers_list, body_start); _ } -> (
          match Std.Net.Http.Version.from_slice version with
          | Error _ -> Error Common.InvalidHttpVersion
          | Ok version ->
              if status_has_no_body status_code then
                let response = response_of_parts status_code version headers_list "" in
                Done { value = response; remaining = body_start }
              else
                match parse_body_framing headers_list with
                | Body_need_more -> Need_more
                | Body_error error -> Error error
                | Body_done CloseDelimitedBody ->
                    let response = response_of_parts status_code version headers_list body_start in
                    Done { value = response; remaining = "" }
                | Body_done (FixedBody content_length) -> (
                    match split_fixed_body body_start content_length with
                    | Body_need_more -> Need_more
                    | Body_error error -> Error error
                    | Body_done (body, remaining) ->
                        let response = response_of_parts status_code version headers_list body in
                        Done { value = response; remaining }
                  )
                | Body_done ChunkedBody -> (
                    match Chunk.decode body_start with
                    | Need_more -> Need_more
                    | Error error -> Error error
                    | Done { value = decoded; _ } ->
                        let response =
                          response_of_parts status_code version headers_list decoded.body
                        in
                        Done { value = response; remaining = decoded.remaining }
                  )
        )
    )

let parse = fun input -> parse_slice (slice_of_string input)
