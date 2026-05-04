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

let parse_status_code = fun code_str ->
  let length = Slice.length code_str in
  if length != 3 then
    Result.Error (Common.StatusCodeLength { length; expected = 3 })
  else
    let rec loop index acc =
      if index >= length then
        if acc >= 100 && acc <= 999 then
          Result.Ok acc
        else
          Result.Error (Common.StatusCodeOutOfRange { code = acc; min = 100; max = 999 })
      else
        let char = Slice.get_unchecked code_str ~at:index in
        let code = Char.to_int char in
        if code < Char.to_int '0' || code > Char.to_int '9' then
          Result.Error (Common.InvalidStatusCodeCharacter { code; index })
        else
          loop (index + 1) ((acc * 10) + (code - Char.to_int '0'))
    in
    loop 0 0

let parse_status_line_slice = fun ?(max_length = 8_192) input ->
  let cursor = Cursor.from_slice input in
  match Cursor.take_until_char cursor '\r' with
  | None ->
      if Slice.length input > max_length then
        Cursor_error (Common.StatusLineTooLong { max_length })
      else
        Cursor_need_more
  | Some (line, cursor) -> (
      if Slice.length line > max_length then
        Cursor_error (Common.StatusLineTooLong { max_length })
      else
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
                        match parse_status_code code_str with
                        | Error error -> Cursor_error (Common.InvalidStatusCode error)
                        | Ok status_code ->
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
  let rec loop acc = fun __tmp1 ->
    match __tmp1 with
    | [] -> List.reverse acc
    | (header_name, value) :: rest ->
        if header_name_equal header_name name then
          loop (value :: acc) rest
        else
          loop acc rest
  in
  loop [] headers

let parse_content_length_value = fun value ->
  match Common.parse_content_length_value value with
  | Error error -> Body_error (Common.InvalidContentLength error)
  | Ok length -> Body_done length

let parse_content_length_values = fun values ->
  let rec loop expected = fun __tmp1 ->
    match __tmp1 with
    | [] -> Body_done expected
    | value :: rest -> (
        match parse_content_length_value value with
        | Body_need_more -> Body_need_more
        | Body_error error -> Body_error error
        | Body_done length -> (
            match expected with
            | None -> loop (Some length) rest
            | Some previous when previous = length -> loop expected rest
            | Some previous ->
                Body_error (Common.ConflictingContentLength { expected = previous; actual = length })
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
  let status = Std.Net.Http.Status.from_int status_code in
  let headers = Std.Net.Http.Header.from_list headers_list in
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

type response_head_owned = {
  head_status_code: int;
  head_version: Std.Net.Http.Version.t;
  head_headers: (string * string) list;
  body_start: string;
}

let parse_head_owned = fun
  ?(max_status_line = 8_192)
  ?(max_headers = 100)
  ?(max_header_length = 8_192)
  ?(max_header_block_length = 65_536)
  input ->
  match parse_status_line_slice ~max_length:max_status_line input with
  | Cursor_need_more -> Cursor_need_more
  | Cursor_error error -> Cursor_error error
  | Cursor_done { value = {
      version;
      status_code;
      reason = _;
      remaining;
    }; _ } ->
      (
          match Request.parse_headers
            ~max_count:max_headers
            ~max_length:max_header_length
            ~max_total_length:max_header_block_length
            remaining with
          | Need_more -> Cursor_need_more
          | Error error -> Cursor_error error
          | Done { value = (headers_list, body_start); _ } -> (
              match Std.Net.Http.Version.from_slice version with
              | Error _ -> Cursor_error Common.InvalidHttpVersion
              | Ok version ->
                  Cursor_done {
                    value =
                      {
                        head_status_code = status_code;
                        head_version = version;
                        head_headers = headers_list;
                        body_start;
                      };
                    remaining;
                  }
            )
        )

let parse_head_slice = fun
  ?(max_status_line = 8_192)
  ?(max_headers = 100)
  ?(max_header_length = 8_192)
  ?(max_header_block_length = 65_536)
  input ->
  match parse_head_owned
    ~max_status_line
    ~max_headers
    ~max_header_length
    ~max_header_block_length
    input with
  | Cursor_need_more -> Need_more
  | Cursor_error error -> Error error
  | Cursor_done { value = {
      head_status_code;
      head_version;
      head_headers;
      body_start;
    }; _ } ->
      let response = response_of_parts head_status_code head_version head_headers "" in
      Done { value = response; remaining = body_start }

let parse_head = fun
  ?(max_status_line = 8_192)
  ?(max_headers = 100)
  ?(max_header_length = 8_192)
  ?(max_header_block_length = 65_536)
  input ->
  match Common.slice_of_string input with
  | Error error -> Error error
  | Ok input ->
      parse_head_slice
        ~max_status_line
        ~max_headers
        ~max_header_length
        ~max_header_block_length
        input

let parse_slice = fun
  ?(max_status_line = 8_192)
  ?(max_headers = 100)
  ?(max_header_length = 8_192)
  ?(max_header_block_length = 65_536)
  ?(max_body_size = Int.max_int)
  ?(max_chunk_size = Int.max_int)
  ?(max_trailers = 100)
  ?(max_trailer_length = 8_192)
  input ->
  match parse_head_owned
    ~max_status_line
    ~max_headers
    ~max_header_length
    ~max_header_block_length
    input with
  | Cursor_need_more -> Need_more
  | Cursor_error error -> Error error
  | Cursor_done { value = {
      head_status_code;
      head_version;
      head_headers;
      body_start;
    }; _ } ->
      if status_has_no_body head_status_code then
        let response = response_of_parts head_status_code head_version head_headers "" in
        Done { value = response; remaining = body_start }
      else
        match parse_body_framing head_headers with
        | Body_need_more -> Need_more
        | Body_error error -> Error error
        | Body_done CloseDelimitedBody ->
            if String.length body_start > max_body_size then
              Error (Common.BodyTooLarge {
                size = String.length body_start;
                max_size = max_body_size;
              })
            else
              let response =
                response_of_parts head_status_code head_version head_headers body_start
              in
              Done { value = response; remaining = "" }
        | Body_done (FixedBody content_length) -> (
            if content_length > max_body_size then
              Error (Common.BodyTooLarge { size = content_length; max_size = max_body_size })
            else
              match split_fixed_body body_start content_length with
              | Body_need_more -> Need_more
              | Body_error error -> Error error
              | Body_done (body, remaining) ->
                  let response =
                    response_of_parts head_status_code head_version head_headers body
                  in
                  Done { value = response; remaining }
          )
        | Body_done ChunkedBody -> (
            match Chunk.decode
              ~max_chunk_size
              ~max_body_size
              ~max_trailers
              ~max_trailer_length
              body_start with
            | Need_more -> Need_more
            | Error error -> Error error
            | Done { value = decoded; _ } ->
                let response =
                  response_of_parts head_status_code head_version head_headers decoded.body
                in
                Done { value = response; remaining = decoded.remaining }
          )

let parse = fun
  ?(max_status_line = 8_192)
  ?(max_headers = 100)
  ?(max_header_length = 8_192)
  ?(max_header_block_length = 65_536)
  ?(max_body_size = Int.max_int)
  ?(max_chunk_size = Int.max_int)
  ?(max_trailers = 100)
  ?(max_trailer_length = 8_192)
  input ->
  match Common.slice_of_string input with
  | Error error -> Error error
  | Ok input ->
      parse_slice
        ~max_status_line
        ~max_headers
        ~max_header_length
        ~max_header_block_length
        ~max_body_size
        ~max_chunk_size
        ~max_trailers
        ~max_trailer_length
        input
