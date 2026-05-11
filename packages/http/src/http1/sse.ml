(** Server-Sent Events Parser *)
open Std
open Std.Iter

module Slice = IO.IoVec.IoSlice

type event = {
  data: string;
  event_type: string option;
  id: string option;
  retry: int option;
}

type partial_event = {
  data_lines_rev: string list;
  event_type: string option;
  id: string option;
  retry: int option;
}

type field = { name: string; value: string }

let empty_partial = {
  data_lines_rev = [];
  event_type = None;
  id = None;
  retry = None;
}

let trim_trailing_cr = fun line ->
  let len = String.length line in
  if len = 0 then
    line
  else if String.get_unchecked line ~at:(len - 1) = '\r' then
    String.sub line ~offset:0 ~len:(len - 1)
  else
    line

let find_colon = fun line ->
  let len = String.length line in
  let rec loop = fun index ->
    if index >= len then
      None
    else if String.get_unchecked line ~at:index = ':' then
      Some index
    else
      loop (index + 1)
  in
  loop 0

let split_field = fun line ->
  let line = trim_trailing_cr line in
  if String.length line = 0 then
    None
  else if String.get_unchecked line ~at:0 = ':' then
    None
  else
    match find_colon line with
    | None -> Some { name = line; value = "" }
    | Some colon_index ->
        let name = String.sub line ~offset:0 ~len:colon_index in
        let value_start =
          let after_colon = colon_index + 1 in
          if after_colon < String.length line then
            if String.get_unchecked line ~at:after_colon = ' ' then
              after_colon + 1
            else
              after_colon
          else
            after_colon
        in
        let value = String.sub line ~offset:value_start ~len:(String.length line - value_start) in
        Some { name; value }

let partial_has_fields = fun partial -> partial.data_lines_rev != []

let finalize_partial = fun partial ->
  if partial_has_fields partial then
    Some {
      data = String.concat "\n" (List.reverse partial.data_lines_rev);
      event_type = partial.event_type;
      id = partial.id;
      retry = partial.retry;
    }
  else
    None

let apply_field = fun partial field ->
  match field.name with
  | "data" -> { partial with data_lines_rev = field.value :: partial.data_lines_rev }
  | "event" -> { partial with event_type = Some field.value }
  | "id" -> { partial with id = Some field.value }
  | "retry" -> (
      match Int.parse field.value with
      | Some retry when retry >= 0 -> { partial with retry = Some retry }
      | Some _
      | None -> partial
    )
  | _ -> partial

let split_lines = fun input ->
  let len = String.length input in
  let rec loop start index acc =
    if index >= len then
      List.reverse (String.sub input ~offset:start ~len:(len - start) :: acc)
    else if String.get_unchecked input ~at:index = '\n' then
      let line = String.sub input ~offset:start ~len:(index - start) in
      loop (index + 1) (index + 1) (line :: acc)
    else
      loop start (index + 1) acc
  in
  loop 0 0 []

let parse_line_slice = fun line ->
  if Slice.length line = 0 then
    None
  else
    let cursor = Cursor.from_slice line in
    match Cursor.take_until cursor (fun c -> c = ':') with
    | None -> None
    | Some (field, cursor) -> (
        let cursor =
          Cursor.advance cursor
          |> Option.unwrap
        in
        let cursor = Cursor.skip_while cursor (fun c -> c = ' ') in
        let value =
          Cursor.remaining cursor
          |> Slice.to_string
        in
        match Slice.to_string field with
        | "" -> None
        | "data" ->
            Some {
              data = value;
              event_type = None;
              id = None;
              retry = None;
            }
        | "event" ->
            Some {
              data = "";
              event_type = Some value;
              id = None;
              retry = None;
            }
        | "id" ->
            Some {
              data = "";
              event_type = None;
              id = Some value;
              retry = None;
            }
        | "retry" -> (
            match Int.parse value with
            | Some retry ->
                Some {
                  data = "";
                  event_type = None;
                  id = None;
                  retry = Some retry;
                }
            | None -> None
          )
        | _ -> None
      )

let parse_line = fun line ->
  match Common.slice_of_string line with
  | Error _ -> None
  | Ok line -> parse_line_slice line

let parse = fun input ->
  let lines = split_lines input in
  let rec loop events partial = fun __tmp1 ->
    match __tmp1 with
    | [] -> (
        match finalize_partial partial with
        | Some event -> List.reverse (event :: events)
        | None -> List.reverse events
      )
    | line :: rest ->
        let line = trim_trailing_cr line in
        if String.length line = 0 then
          match finalize_partial partial with
          | Some event -> loop (event :: events) empty_partial rest
          | None -> loop events empty_partial rest
        else
          match split_field line with
          | None -> loop events partial rest
          | Some field -> loop
            events
            (apply_field partial field)
            rest
  in
  loop [] empty_partial lines
