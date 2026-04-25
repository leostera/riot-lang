(** Server-Sent Events Parser *)
open Std
open Std.Iter

module Slice = IO.IoVec.IoSlice

type event = { data: string; event_type: string option; id: string option; retry: int option }

let slice_of_string = fun value ->
  match Slice.from_string value with
  | Ok slice -> slice
  | Error error -> panic ("Http1.Sse.slice_of_string: " ^ Slice.error_message error)

let parse_line_slice = fun line ->
  let line_cursor = Cursor.from_slice line in
  let line_cursor =
    Cursor.skip_while line_cursor
      (
        fun c -> c = ' ' || c = '\t'
      )
  in
  let line = Cursor.remaining line_cursor in
  if Slice.length line = 0 then
    None
  else
    let cursor = Cursor.from_slice line in
    match Cursor.take_until cursor
      (
        fun c -> c = ':'
      ) with
    | None -> None
    | Some (field, cursor) -> (
      let cursor = Cursor.advance cursor |> Option.unwrap in
      let cursor =
        Cursor.skip_while cursor
          (
            fun c -> c = ' '
          )
      in
      let value = Cursor.remaining cursor |> Slice.to_string in
      match Slice.to_string field with
      | "" -> None
      | "data" ->
          Some {
            data = value;
            event_type = None;
            id = None;
            retry = None
          }
      | "event" ->
          Some {
            data = "";
            event_type = Some value;
            id = None;
            retry = None
          }
      | "id" ->
          Some {
            data = "";
            event_type = None;
            id = Some value;
            retry = None
          }
      | "retry" -> (
        match Int.parse value with
        | Some retry ->
            Some {
              data = "";
              event_type = None;
              id = None;
              retry = Some retry
            }
        | None -> None
      )
      | _ -> None
    )

let parse_line = fun line -> parse_line_slice (slice_of_string line)
