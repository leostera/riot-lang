open Std

type event = {
  data: string;
  event_type: string option;
  id: string option;
}

type parsed =
  | Event of event
  | Skip
  | Done

let delimiter = fun buffer ->
  let buffer_len = String.length buffer in
  let matches pattern offset =
    let pattern_len = String.length pattern in
    if offset + pattern_len > buffer_len then
      false
    else
      let rec loop index =
        if index >= pattern_len then
          true
        else if
          Char.equal
            (String.get_unchecked buffer ~at:(offset + index))
            (String.get_unchecked pattern ~at:index)
        then
          loop (index + 1)
        else
          false
      in
      loop 0
  in
  let rec loop offset =
    if offset >= buffer_len then
      None
    else if matches "\r\n\r\n" offset then
      Some (offset, 4)
    else if matches "\n\n" offset || matches "\r\r" offset then
      Some (offset, 2)
    else
      loop (offset + 1)
  in
  loop 0

let split_event = fun buffer ->
  match delimiter buffer with
  | None -> None
  | Some (offset, delimiter_len) ->
      let event_text = String.sub buffer ~offset:0 ~len:offset in
      let remaining =
        String.sub
          buffer
          ~offset:(offset + delimiter_len)
          ~len:(String.length buffer - offset - delimiter_len)
      in
      Some (event_text, remaining)

let strip_trailing_cr = fun line ->
  let len = String.length line in
  if len = 0 then
    line
  else if String.get_unchecked line ~at:(len - 1) = '\r' then
    String.sub line ~offset:0 ~len:(len - 1)
  else
    line

let find_colon = fun line ->
  let len = String.length line in
  let rec loop offset =
    if offset >= len then
      None
    else if String.get_unchecked line ~at:offset = ':' then
      Some offset
    else
      loop (offset + 1)
  in
  loop 0

let field_value = fun line colon_at ->
  let len = String.length line in
  let start =
    let after_colon = colon_at + 1 in
    if after_colon < len then
      if String.get_unchecked line ~at:after_colon = ' ' then
        after_colon + 1
      else
        after_colon
    else
      after_colon
  in
  String.sub line ~offset:start ~len:(len - start)

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

let parse_event = fun buffer ->
  match split_event buffer with
  | None -> None
  | Some ("", remaining) -> Some (Skip, remaining)
  | Some (event_text, remaining) ->
      let lines = split_lines event_text in
      let data_lines = ref [] in
      let event_type = ref None in
      let id = ref None in
      let saw_event_field = ref false in
      List.for_each
        lines
        ~fn:(fun raw_line ->
          let line = strip_trailing_cr raw_line in
          if String.length line = 0 then
            ()
          else if String.get_unchecked line ~at:0 = ':' then
            ()
          else
            match find_colon line with
            | None -> ()
            | Some colon_at ->
                let field = String.sub line ~offset:0 ~len:colon_at in
                let value = field_value line colon_at in
                (
                  match field with
                  | "data" ->
                      saw_event_field := true;
                      data_lines := value :: !data_lines
                  | "event" ->
                      saw_event_field := true;
                      event_type := Some value
                  | "id" ->
                      saw_event_field := true;
                      id := Some value
                  | _ -> ()
                ));
      let data = String.concat "\n" (List.reverse !data_lines) in
      if String.equal data "[DONE]" then
        Some (Done, remaining)
      else if !saw_event_field then
        Some (Event { data; event_type = !event_type; id = !id }, remaining)
      else
        Some (Skip, remaining)

module SSEIterator = struct
  type state = {
    conn: Connection.t;
    mutable buffer: string;
    mutable done_: bool;
  }

  type item = (event, Error.t) result

  let rec next = fun state ->
    match parse_event state.buffer with
    | Some (Event event, remaining) ->
        state.buffer <- remaining;
        Some (Ok event)
    | Some (Skip, remaining) ->
        state.buffer <- remaining;
        next state
    | Some (Done, remaining) ->
        state.buffer <- remaining;
        state.done_ <- true;
        None
    | None -> (
        if state.done_ then
          if String.equal state.buffer "" then
            None
          else (
            state.buffer <- "";
            Some (Error (Error.ProtocolError Error.IncompleteSseEvent))
          )
        else
          match Connection.stream state.conn with
          | Error error ->
              state.done_ <- true;
              Some (Error error)
          | Ok msgs ->
              List.for_each
                msgs
                ~fn:(fun msg ->
                  match msg with
                  | Connection.Data chunk -> state.buffer <- state.buffer ^ chunk
                  | Connection.Done -> state.done_ <- true
                  | Connection.Status _
                  | Connection.Headers _ -> ());
              next state
      )

  let size = fun _state -> 0

  let clone = fun state -> { conn = state.conn; buffer = state.buffer; done_ = state.done_ }
end

let await = fun conn ->
  let module I = SSEIterator in
  Iter.MutIterator.make (module I) { I.conn; buffer = ""; done_ = false }
