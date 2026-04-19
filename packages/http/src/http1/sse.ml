(** Server-Sent Events Parser *)
open Std
open Std.Iter

let ( let* ) = Result.and_then

type event = {
  data: string;
  event_type: string option;
  id: string option;
  retry: int option;
}

let parse_line = fun line ->
  (* Trim line using Cursor *)
  let line_cursor = Cursor.create line in
  let line_cursor =
    Cursor.skip_while line_cursor (fun c -> c = ' ' || c = '\t')
  in
  let line = Cursor.remaining_string line_cursor in
  if line = "" then
    None
  else
    let cursor = Cursor.create line in
    (* Take until colon to get field name *)
    match Cursor.take_until_string cursor (fun c -> c = ':') with
    | None -> None
    | Some (field, cursor) -> (
        (* Skip colon *)
        let cursor = Cursor.advance cursor |> Option.unwrap in
        (* Skip optional space after colon *)
        let cursor =
          Cursor.skip_while cursor (fun c -> c = ' ')
        in
        let value = Cursor.remaining_string cursor in
        match field with
        | "" ->
            None
        | "data" ->
            Some { data = value; event_type = None; id = None; retry = None }
        | "event" ->
            Some { data = ""; event_type = Some value; id = None; retry = None }
        | "id" ->
            Some { data = ""; event_type = None; id = Some value; retry = None }
        | "retry" -> (
            match Int.parse value with
            | Some retry -> Some { data = ""; event_type = None; id = None; retry = Some retry }
            | None -> None
          )
        | _ ->
            None
      )
