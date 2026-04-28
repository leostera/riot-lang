(** Chunked Transfer Encoding Parser *)
open Std
open Std.Iter
open Common

module Slice = IO.IoVec.IoSlice

type chunk_result = { data: string; remaining: string }

type body_result = {
  body: string;
  trailers: (string * string) list;
  remaining: string;
}

type 'a cursor_parse_result =
  | Cursor_done of {
      value: 'a;
      remaining: Cursor.t;
    }
  | Cursor_need_more
  | Cursor_error of Common.error

let take_crlf = fun error cursor ->
  match Cursor.take_n cursor 2 with
  | None -> Cursor_need_more
  | Some (ending, cursor) when Slice.equal_string ending "\r\n" ->
      Cursor_done { value = (); remaining = cursor }
  | Some _ -> Cursor_error error

let is_optional_whitespace = fun c -> c = ' ' || c = '\t'

let validate_chunk_extension = fun line semicolon_index ->
  let rec loop index =
    if index >= String.length line then
      Result.Ok ()
    else
      let c = String.get_unchecked line ~at:index in
      let code = Char.to_int c in
      if c = '\t' || (code >= 0x20 && code <= 0x7e) || code >= 0x80 then
        loop (index + 1)
      else
        Result.Error (Common.InvalidChunkExtensionCharacter { code; index })
  in
  loop (semicolon_index + 1)

let split_chunk_size_line = fun line ->
  match Common.find_substring ~needle:";" line with
  | None -> Result.Ok (String.trim line)
  | Some semicolon_index -> (
      match validate_chunk_extension line semicolon_index with
      | Error error -> Result.Error error
      | Ok () ->
          Result.Ok (
            String.sub line ~offset:0 ~len:semicolon_index
            |> String.trim
          )
    )

let hex_digit = fun c ->
  let code = Char.to_int c in
  if code >= Char.to_int '0' && code <= Char.to_int '9' then
    Some (code - Char.to_int '0')
  else if code >= Char.to_int 'a' && code <= Char.to_int 'f' then
    Some (code - Char.to_int 'a' + 10)
  else if code >= Char.to_int 'A' && code <= Char.to_int 'F' then
    Some (code - Char.to_int 'A' + 10)
  else
    None

let parse_chunk_size = fun size_hex ->
  let length = String.length size_hex in
  if length = 0 then
    Result.Error Common.EmptyChunkSize
  else
    let rec loop index acc =
      if index >= length then
        Result.Ok acc
      else
        let char = String.get_unchecked size_hex ~at:index in
        match hex_digit char with
        | None ->
            Result.Error (Common.InvalidChunkSizeCharacter { code = Char.to_int char; index })
        | Some digit ->
            if acc > (Int.max_int - digit) / 16 then
              Result.Error Common.ChunkSizeOverflow
            else
              loop (index + 1) ((acc * 16) + digit)
    in
    loop 0 0

let parse_size = fun ?(max_line_length = 8_192) cursor ->
  match Cursor.take_until_char cursor '\r' with
  | None ->
      if Slice.length (Cursor.remaining cursor) > max_line_length then
        Cursor_error (Common.ChunkSizeLineTooLong { max_length = max_line_length })
      else
        Cursor_need_more
  | Some (size_hex, cursor) -> (
      if Slice.length size_hex > max_line_length then
        Cursor_error (Common.ChunkSizeLineTooLong { max_length = max_line_length })
      else
        match take_crlf Common.InvalidChunkSizeLineEnding cursor with
        | Cursor_need_more -> Cursor_need_more
        | Cursor_error error -> Cursor_error error
        | Cursor_done { remaining = cursor; _ } -> (
            let size_line = Slice.to_string size_hex in
            match split_chunk_size_line size_line with
            | Error error -> Cursor_error error
            | Ok size_hex -> (
                match parse_chunk_size size_hex with
                | Ok size -> Cursor_done { value = size; remaining = cursor }
                | Error error -> Cursor_error (Common.InvalidChunkSize error)
              )
          )
    )

let parse_slice = fun ?(max_chunk_size_line = 8_192) input ->
  let cursor = Cursor.from_slice input in
  match parse_size ~max_line_length:max_chunk_size_line cursor with
  | Cursor_need_more -> Need_more
  | Cursor_error error -> Error error
  | Cursor_done { value = 0; remaining } ->
      Done {
        value = { data = ""; remaining = Slice.to_string (Cursor.remaining remaining) };
        remaining = "";
      }
  | Cursor_done { value = size; remaining } -> (
      match Cursor.take_n remaining size with
      | None -> Need_more
      | Some (data, cursor) -> (
          match take_crlf Common.InvalidChunkDataLineEnding cursor with
          | Cursor_need_more -> Need_more
          | Cursor_error error -> Error error
          | Cursor_done { remaining = cursor; _ } ->
              Done {
                value = {
                  data = Slice.to_string data;
                  remaining = Slice.to_string (Cursor.remaining cursor);
                };
                remaining = "";
              }
        )
    )

let parse = fun ?(max_chunk_size_line = 8_192) input ->
  match Common.slice_of_string input with
  | Error error -> Error error
  | Ok input -> parse_slice ~max_chunk_size_line input

type trailer_line = {
  trailer_name: string;
  trailer_value: string;
  next_cursor: Cursor.t;
}

let take_trailer_block_terminator = fun cursor ->
  match Cursor.take_n cursor 2 with
  | Some (prefix, cursor) when Slice.equal_string prefix "\r\n" -> Some cursor
  | _ -> None

let parse_trailer_line = fun cursor ->
  match Cursor.take_until_char cursor '\r' with
  | None -> Cursor_need_more
  | Some (line, cursor) -> (
      match take_crlf Common.InvalidCrlf cursor with
      | Cursor_need_more -> Cursor_need_more
      | Cursor_error error -> Cursor_error error
      | Cursor_done { remaining = cursor; _ } -> (
          if Slice.length line > 0 && is_optional_whitespace (Slice.get_unchecked line ~at:0) then
            Cursor_error (Common.InvalidHeaderFormat Common.ObsoleteLineFolding)
          else
            let line_string = Slice.to_string line in
            match Common.find_substring ~needle:":" line_string with
            | None -> Cursor_error (Common.InvalidHeaderFormat Common.MissingColon)
            | Some colon_index ->
                let name = String.sub line_string ~offset:0 ~len:colon_index in
                let raw_value =
                  String.sub
                    line_string
                    ~offset:(colon_index + 1)
                    ~len:(String.length line_string - colon_index - 1)
                in
                let value = String.trim raw_value in
                match Common.validate_header_name name with
                | Error error -> Cursor_error (Common.InvalidHeaderFormat error)
                | Ok () -> (
                    match Common.validate_header_value value with
                    | Error error -> Cursor_error (Common.InvalidHeaderFormat error)
                    | Ok () ->
                        Cursor_done {
                          value = {
                            trailer_name = name;
                            trailer_value = value;
                            next_cursor = cursor;
                          };
                          remaining = cursor;
                        }
                  )
        )
    )

let rec parse_trailers = fun
  ?(max_count = 100)
  ?(max_length = 8_192)
  ?(acc = [])
  ?(count = 0)
  cursor ->
  match take_trailer_block_terminator cursor with
  | Some cursor -> Cursor_done { value = List.reverse acc; remaining = cursor }
  | None ->
      if count >= max_count then
        Cursor_error (Common.TooManyHeaders { max_count })
      else
        match parse_trailer_line cursor with
        | Cursor_need_more ->
            if Slice.length (Cursor.remaining cursor) > max_length then
              Cursor_error (Common.HeaderTooLong { max_length })
            else
              Cursor_need_more
        | Cursor_error error -> Cursor_error error
        | Cursor_done { value = { trailer_name; trailer_value; next_cursor }; _ } ->
            if String.length trailer_name + String.length trailer_value > max_length then
              Cursor_error (Common.HeaderTooLong { max_length })
            else
              parse_trailers
                ~max_count
                ~max_length
                ~acc:((trailer_name, trailer_value) :: acc)
                ~count:(count + 1)
                next_cursor

let decode_slice = fun
  ?(max_chunk_size = Int.max_int)
  ?(max_chunk_size_line = 8_192)
  ?(max_body_size = Int.max_int)
  ?(max_trailers = 100)
  ?(max_trailer_length = 8_192)
  input ->
  let rec loop chunks body_size cursor =
    match parse_size ~max_line_length:max_chunk_size_line cursor with
    | Cursor_need_more -> Need_more
    | Cursor_error error -> Error error
    | Cursor_done { value = 0; remaining } -> (
        match parse_trailers ~max_count:max_trailers ~max_length:max_trailer_length remaining with
        | Cursor_need_more -> Need_more
        | Cursor_error error -> Error error
        | Cursor_done { value = trailers; remaining } ->
            Done {
              value = {
                body = String.concat "" (List.reverse chunks);
                trailers;
                remaining = Slice.to_string (Cursor.remaining remaining);
              };
              remaining = "";
            }
      )
    | Cursor_done { value = size; remaining } ->
        if size > max_chunk_size then
          Error (Common.ChunkTooLarge { size; max_size = max_chunk_size })
        else if size > max_body_size - body_size then
          Error (Common.ChunkedBodyTooLarge { size = body_size + size; max_size = max_body_size })
        else
          match Cursor.take_n remaining size with
          | None -> Need_more
          | Some (data, cursor) -> (
              match take_crlf Common.InvalidChunkDataLineEnding cursor with
              | Cursor_need_more -> Need_more
              | Cursor_error error -> Error error
              | Cursor_done { remaining = cursor; _ } ->
                  loop (Slice.to_string data :: chunks) (body_size + size) cursor
            )
  in
  loop [] 0 (Cursor.from_slice input)

let decode = fun
  ?(max_chunk_size = Int.max_int)
  ?(max_chunk_size_line = 8_192)
  ?(max_body_size = Int.max_int)
  ?(max_trailers = 100)
  ?(max_trailer_length = 8_192)
  input ->
  match Common.slice_of_string input with
  | Error error -> Error error
  | Ok input ->
      decode_slice
        ~max_chunk_size
        ~max_chunk_size_line
        ~max_body_size
        ~max_trailers
        ~max_trailer_length
        input
