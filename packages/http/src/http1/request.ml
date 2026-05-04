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

  let from_cursor = fun cursor ->
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

let string_of_slice = Slice.to_string

let is_space = fun c -> c = ' '

let is_optional_whitespace = fun c -> c = ' ' || c = '\t'

type 'a slice_parse_result =
  | Slice_done of 'a
  | Slice_need_more
  | Slice_error of Common.error

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
  | Error error -> Slice_error (Common.InvalidContentLength error)
  | Ok length -> Slice_done length

let parse_content_length_values = fun values ->
  let rec loop expected = fun __tmp1 ->
    match __tmp1 with
    | [] -> Slice_done expected
    | value :: rest -> (
        match parse_content_length_value value with
        | Slice_need_more -> Slice_need_more
        | Slice_error error -> Slice_error error
        | Slice_done length -> (
            match expected with
            | None -> loop (Some length) rest
            | Some previous when previous = length -> loop expected rest
            | Some previous ->
                Slice_error (Common.ConflictingContentLength {
                  expected = previous;
                  actual = length;
                })
          )
      )
  in
  loop None values

type body_framing =
  | NoBody
  | FixedBody of int
  | ChunkedBody

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
  | ([], values) -> (
      match parse_content_length_values values with
      | Slice_need_more -> Slice_need_more
      | Slice_error error -> Slice_error error
      | Slice_done None -> Slice_done NoBody
      | Slice_done (Some length) -> Slice_done (FixedBody length)
    )
  | (values, []) when transfer_encoding_is_chunked values -> Slice_done ChunkedBody
  | (_ :: _, []) -> Slice_error Common.UnsupportedTransferEncoding
  | (_ :: _, _ :: _) -> Slice_error Common.TransferEncodingWithContentLength

let split_fixed_body = fun input length ->
  let available = Slice.length input in
  if available < length then
    Slice_need_more
  else
    let body = Slice.sub_unchecked input ~off:0 ~len:length in
    let remaining = Slice.sub_unchecked input ~off:length ~len:(available - length) in
    Slice_done (body, string_of_slice remaining)

type request_line_owned = {
  parsed_method: Std.Net.Http.Method.t;
  parsed_uri: Std.Net.Uri.t;
  parsed_version: Std.Net.Http.Version.t;
  next_cursor: SliceCursor.t;
}

type request_head_owned = {
  head_method: Std.Net.Http.Method.t;
  head_uri: Std.Net.Uri.t;
  head_version: Std.Net.Http.Version.t;
  head_headers: (string * string) list;
  body_cursor: SliceCursor.t;
}

type header_line_owned = {
  header_name: string;
  header_value: string;
  next_cursor: SliceCursor.t;
}

let skip_crlf = fun cursor ->
  match SliceCursor.take_n cursor 2 with
  | None -> Slice_need_more
  | Some (ending, cursor) when Slice.equal_string ending "\r\n" -> Slice_done cursor
  | Some _ -> Slice_error Common.InvalidCrlf

let take_header_block_terminator = fun cursor ->
  match SliceCursor.take_n cursor 2 with
  | Some (prefix, cursor) when Slice.equal_string prefix "\r\n" -> Some cursor
  | _ -> None

let is_tchar = fun c ->
  let code = Char.to_int c in
  (code >= Char.to_int '0' && code <= Char.to_int '9')
  || (code >= Char.to_int 'A' && code <= Char.to_int 'Z')
  || (code >= Char.to_int 'a' && code <= Char.to_int 'z')
  || c = '!'
  || c = '#'
  || c = '$'
  || c = '%'
  || c = '&'
  || c = '\''
  || c = '*'
  || c = '+'
  || c = '-'
  || c = '.'
  || c = '^'
  || c = '_'
  || c = '`'
  || c = '|'
  || c = '~'

let validate_header_name = fun name ->
  if Slice.length name = 0 then
    Slice_error (Common.InvalidHeaderFormat Common.EmptyName)
  else
    let rec loop index =
      if index >= Slice.length name then
        Slice_done ()
      else
        let c = Slice.get_unchecked name ~at:index in
        if c = ' ' || c = '\t' then
          Slice_error (Common.InvalidHeaderFormat Common.WhitespaceBeforeColon)
        else if is_tchar c then
          loop (index + 1)
        else
          Slice_error (Common.InvalidHeaderFormat (Common.InvalidNameCharacter {
            code = Char.to_int c;
            index;
          }))
    in
    loop 0

let validate_header_value = fun value ->
  let rec loop index =
    if index >= Slice.length value then
      Slice_done ()
    else
      let c = Slice.get_unchecked value ~at:index in
      let code = Char.to_int c in
      if c = '\t' || (code >= 0x20 && code <= 0x7e) || code >= 0x80 then
        loop (index + 1)
      else
        Slice_error (Common.InvalidHeaderFormat (Common.InvalidValueCharacter { code; index }))
  in
  loop 0

let parse_request_line_owned = fun ?(max_length = 8_192) input ->
  let cursor = SliceCursor.create input in
  match SliceCursor.take_until_char cursor '\r' with
  | None ->
      if Slice.length input > max_length then
        Slice_error (Common.RequestLineTooLong { max_length })
      else
        Slice_need_more
  | Some (line, cursor) ->
      if Slice.length line > max_length then
        Slice_error (Common.RequestLineTooLong { max_length })
      else
        match skip_crlf cursor with
        | Slice_need_more
        | Slice_error _ as result -> result
        | Slice_done cursor -> (
            let line_cursor = SliceCursor.create line in
            match SliceCursor.take_until_char line_cursor ' ' with
            | None -> Slice_error Common.MissingMethod
            | Some (method_, line_cursor) ->
                let line_cursor = SliceCursor.skip_while line_cursor is_space in
                match SliceCursor.take_until_char line_cursor ' ' with
                | None -> Slice_error Common.MissingPath
                | Some (path, line_cursor) ->
                    let version =
                      SliceCursor.skip_while line_cursor is_space
                      |> SliceCursor.remaining
                    in
                    if not (Slice.starts_with version ~prefix:"HTTP/") then
                      Slice_error Common.InvalidHttpVersion
                    else
                      let method_ = Std.Net.Http.Method.from_slice method_ in
                      match Std.Net.Uri.from_slice path with
                      | Error error -> Slice_error (Common.InvalidRequestTarget error)
                      | Ok uri -> (
                          match Std.Net.Http.Version.from_slice version with
                          | Error Std.Net.Http.Version.InvalidVersion ->
                              Slice_error Common.InvalidHttpVersion
                          | Ok version ->
                              Slice_done {
                                parsed_method = method_;
                                parsed_uri = uri;
                                parsed_version = version;
                                next_cursor = cursor;
                              }
                        )
          )

let parse_header_line_owned = fun cursor ->
  match SliceCursor.take_until_char cursor '\r' with
  | None -> Slice_need_more
  | Some (line, cursor) -> (
      match skip_crlf cursor with
      | Slice_need_more
      | Slice_error _ as result -> result
      | Slice_done cursor -> (
          let line_cursor = SliceCursor.create line in
          if Slice.length line > 0 && is_optional_whitespace (Slice.get_unchecked line ~at:0) then
            Slice_error (Common.InvalidHeaderFormat Common.ObsoleteLineFolding)
          else
            match SliceCursor.take_until_char line_cursor ':' with
            | None -> Slice_error (Common.InvalidHeaderFormat Common.MissingColon)
            | Some (name, line_cursor) -> (
                match SliceCursor.advance line_cursor with
                | None -> Slice_error (Common.InvalidHeaderFormat Common.MissingValueSeparator)
                | Some line_cursor ->
                    let value =
                      SliceCursor.skip_while line_cursor is_optional_whitespace
                      |> SliceCursor.remaining
                    in
                    match validate_header_name name with
                    | Slice_need_more -> Slice_need_more
                    | Slice_error error -> Slice_error error
                    | Slice_done () -> (
                        match validate_header_value value with
                        | Slice_need_more -> Slice_need_more
                        | Slice_error error -> Slice_error error
                        | Slice_done () ->
                            Slice_done {
                              header_name = string_of_slice name;
                              header_value = string_of_slice value;
                              next_cursor = cursor;
                            }
                      )
              )
        )
    )

let rec parse_headers_owned = fun
  ?(max_count = 100)
  ?(max_length = 8_192)
  ?(max_total_length = 65_536)
  ?(acc = [])
  ?(count = 0)
  ?(total_length = 0)
  cursor ->
  match take_header_block_terminator cursor with
  | Some next_cursor ->
      let total_length = total_length + (next_cursor.pos - cursor.pos) in
      if total_length > max_total_length then
        Slice_error (Common.HeaderBlockTooLong { max_length = max_total_length })
      else
        Slice_done (List.reverse acc, next_cursor)
  | None ->
      if count >= max_count then
        Slice_error (Common.TooManyHeaders { max_count })
      else
        match parse_header_line_owned cursor with
        | Slice_need_more ->
            let pending = Slice.length (SliceCursor.remaining cursor) in
            if pending > max_length then
              Slice_error (Common.HeaderTooLong { max_length })
            else if total_length + pending > max_total_length then
              Slice_error (Common.HeaderBlockTooLong { max_length = max_total_length })
            else
              Slice_need_more
        | Slice_error error -> Slice_error error
        | Slice_done { header_name; header_value; next_cursor } ->
            if String.length header_name + String.length header_value > max_length then
              Slice_error (Common.HeaderTooLong { max_length })
            else
              let total_length = total_length + (next_cursor.pos - cursor.pos) in
              if total_length > max_total_length then
                Slice_error (Common.HeaderBlockTooLong { max_length = max_total_length })
              else
                parse_headers_owned
                  ~max_count
                  ~max_length
                  ~max_total_length
                  ~acc:((header_name, header_value) :: acc)
                  ~count:(count + 1)
                  ~total_length
                  next_cursor

let parse_headers = fun
  ?(max_count = 100) ?(max_length = 8_192) ?(max_total_length = 65_536) ?(acc = []) cursor ->
  let cursor = SliceCursor.from_cursor cursor in
  match parse_headers_owned
    ~max_count
    ~max_length
    ~max_total_length
    ~acc
    ~count:(List.length acc)
    cursor with
  | Slice_need_more -> Common.Need_more
  | Slice_error error -> Common.Error error
  | Slice_done (headers, remaining) ->
      Common.Done {
        value = (headers, string_of_slice (SliceCursor.remaining remaining));
        remaining = "";
      }

let request_of_parts = fun method_ uri version headers_list body ->
  let headers = Std.Net.Http.Header.from_list headers_list in
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

let parse_head_owned = fun
  ?(max_request_line = 8_192)
  ?(max_headers = 100)
  ?(max_header_length = 8_192)
  ?(max_header_block_length = 65_536)
  input ->
  match parse_request_line_owned ~max_length:max_request_line input with
  | Slice_need_more -> Slice_need_more
  | Slice_error error -> Slice_error error
  | Slice_done {
      parsed_method;
      parsed_uri;
      parsed_version;
      next_cursor;
    } ->
      (
          match parse_headers_owned
            ~max_count:max_headers
            ~max_length:max_header_length
            ~max_total_length:max_header_block_length
            next_cursor with
          | Slice_need_more -> Slice_need_more
          | Slice_error error -> Slice_error error
          | Slice_done (headers_list, body_cursor) ->
              Slice_done {
                head_method = parsed_method;
                head_uri = parsed_uri;
                head_version = parsed_version;
                head_headers = headers_list;
                body_cursor;
              }
        )

let parse_head_slice = fun
  ?(max_request_line = 8_192)
  ?(max_headers = 100)
  ?(max_header_length = 8_192)
  ?(max_header_block_length = 65_536)
  input ->
  match parse_head_owned
    ~max_request_line
    ~max_headers
    ~max_header_length
    ~max_header_block_length
    input with
  | Slice_need_more -> Common.Need_more
  | Slice_error error -> Common.Error error
  | Slice_done {
      head_method;
      head_uri;
      head_version;
      head_headers;
      body_cursor;
    } ->
      let request = request_of_parts head_method head_uri head_version head_headers Slice.empty in
      Common.Done {
        value = request;
        remaining = string_of_slice (SliceCursor.remaining body_cursor);
      }

let parse_head = fun
  ?(max_request_line = 8_192)
  ?(max_headers = 100)
  ?(max_header_length = 8_192)
  ?(max_header_block_length = 65_536)
  input ->
  match Common.slice_of_string input with
  | Error error -> Common.Error error
  | Ok input ->
      parse_head_slice
        ~max_request_line
        ~max_headers
        ~max_header_length
        ~max_header_block_length
        input

let parse_slice = fun
  ?(max_request_line = 8_192)
  ?(max_headers = 100)
  ?(max_header_length = 8_192)
  ?(max_header_block_length = 65_536)
  ?(max_body_size = Int.max_int)
  ?(max_chunk_size = Int.max_int)
  ?(max_trailers = 100)
  ?(max_trailer_length = 8_192)
  input ->
  match parse_head_owned
    ~max_request_line
    ~max_headers
    ~max_header_length
    ~max_header_block_length
    input with
  | Slice_need_more -> Common.Need_more
  | Slice_error error -> Common.Error error
  | Slice_done {
      head_method;
      head_uri;
      head_version;
      head_headers;
      body_cursor;
    } ->
      (
          let body_bytes = SliceCursor.remaining body_cursor in
          match parse_body_framing head_headers with
          | Slice_need_more -> Common.Need_more
          | Slice_error error -> Common.Error error
          | Slice_done NoBody ->
              let request =
                request_of_parts head_method head_uri head_version head_headers Slice.empty
              in
              Common.Done { value = request; remaining = string_of_slice body_bytes }
          | Slice_done (FixedBody content_length) -> (
              if content_length > max_body_size then
                Common.Error (Common.BodyTooLarge {
                  size = content_length;
                  max_size = max_body_size;
                })
              else
                match split_fixed_body body_bytes content_length with
                | Slice_need_more -> Common.Need_more
                | Slice_error error -> Common.Error error
                | Slice_done (body, remaining) ->
                    let request =
                      request_of_parts head_method head_uri head_version head_headers body
                    in
                    Common.Done { value = request; remaining }
            )
          | Slice_done ChunkedBody -> (
              match Chunk.decode_slice
                ~max_chunk_size
                ~max_body_size
                ~max_trailers
                ~max_trailer_length
                body_bytes with
              | Common.Need_more -> Common.Need_more
              | Common.Error error -> Common.Error error
              | Common.Done { value = decoded; _ } -> (
                  match Common.slice_of_string decoded.body with
                  | Error error -> Common.Error error
                  | Ok body ->
                      let request =
                        request_of_parts head_method head_uri head_version head_headers body
                      in
                      Common.Done { value = request; remaining = decoded.remaining }
                )
            )
        )

let parse = fun
  ?(max_request_line = 8_192)
  ?(max_headers = 100)
  ?(max_header_length = 8_192)
  ?(max_header_block_length = 65_536)
  ?(max_body_size = Int.max_int)
  ?(max_chunk_size = Int.max_int)
  ?(max_trailers = 100)
  ?(max_trailer_length = 8_192)
  input ->
  match Common.slice_of_string input with
  | Error error -> Common.Error error
  | Ok input ->
      parse_slice
        ~max_request_line
        ~max_headers
        ~max_header_length
        ~max_header_block_length
        ~max_body_size
        ~max_chunk_size
        ~max_trailers
        ~max_trailer_length
        input
