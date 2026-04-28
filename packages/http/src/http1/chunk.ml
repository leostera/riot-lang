(** Chunked Transfer Encoding Parser *)
open Std
open Std.Iter
open Common

module Slice = IO.IoVec.IoSlice

type chunk_result = { data: string; remaining: string }

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
  | Error error -> panic ("Http1.Chunk.slice_of_string: " ^ Slice.error_message error)

let take_crlf = fun error cursor ->
  match Cursor.take_n cursor 2 with
  | None -> Cursor_need_more
  | Some (ending, cursor) when Slice.equal_string ending "\r\n" ->
      Cursor_done { value = (); remaining = cursor }
  | Some _ -> Cursor_error error

let parse_size = fun cursor ->
  match Cursor.take_until_char cursor '\r' with
  | None -> Cursor_need_more
  | Some (size_hex, cursor) -> (
      match take_crlf Common.InvalidChunkSizeLineEnding cursor with
      | Cursor_need_more -> Cursor_need_more
      | Cursor_error error -> Cursor_error error
      | Cursor_done { remaining = cursor; _ } -> (
          match Int.parse ("0x" ^ Slice.to_string size_hex) with
          | Some size -> Cursor_done { value = size; remaining = cursor }
          | None -> Cursor_error Common.InvalidChunkSize
        )
    )

let parse_slice = fun input ->
  let cursor = Cursor.from_slice input in
  match parse_size cursor with
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

let parse = fun input -> parse_slice (slice_of_string input)
