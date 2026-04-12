(** Chunked Transfer Encoding Parser *)
open Std
open Std.Iter
open Common

let ( let* ) = Result.and_then

type chunk_result = {
  data: string;
  remaining: string;
}

let parse_size = fun cursor ->
  match Cursor.take_until cursor (fun c -> c = '\r') with
  | None -> Need_more
  | Some (size_hex, cursor) -> (
      match Cursor.advance_by cursor 2 with
      | None -> Error "Invalid chunk size line ending"
      | Some cursor -> (
          match Int.parse_opt ("0x" ^ size_hex) with
          | Some size -> Done { value = size; remaining = Cursor.remaining cursor }
          | None -> Error "Invalid chunk size"
        )
    )

let parse = fun input ->
  let cursor = Cursor.create input in
  match parse_size cursor with
  | Need_more ->
      Need_more
  | Error e ->
      Error e
  | Done { value=0; remaining } ->
      Done { value = { data = ""; remaining }; remaining = "" }
  | Done { value=size; remaining } -> (
      let cursor = Cursor.create remaining in
      match Cursor.take_n cursor size with
      | None -> Need_more
      | Some (data, cursor) -> (
          match Cursor.advance_by cursor 2 with
          | None -> Need_more
          | Some cursor -> Done {
            value = { data; remaining = Cursor.remaining cursor };
            remaining = ""
          }
        )
    )
