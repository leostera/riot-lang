open Std

type event = { data: string; event_type: string option; id: string option }

(* Internal state for SSE iterator *)
module SSEIterator = struct
  type state = { conn: Connection.t; mutable buffer: string; mutable done_: bool }

  type item = event

  (* Split string on first occurrence of pattern *)
  let split_on_pattern = fun pattern str ->
    let pattern_len = String.length pattern in
    let rec find_pattern pos =
      if pos + pattern_len > String.length str then
        None
      else
        if String.sub str ~offset:pos ~len:pattern_len = pattern then
          Some pos
        else find_pattern (pos + 1)
    in
    match find_pattern 0 with
    | None -> None
    | Some pos ->
        let before = String.sub str ~offset:0 ~len:pos in
        let after = String.sub str ~offset:(pos + pattern_len) ~len:(String.length str - pos - pattern_len) in Some (before, after)

  (* Parse one complete SSE event from buffer *)
  let parse_event = fun buffer ->
    (* SSE events are separated by "\n\n" *)
    match split_on_pattern "\n\n" buffer with
    | None -> None
    | Some ("", remaining) -> (* Empty event, skip it and continue *)
    Some (None, remaining)
    | Some (event_str, remaining) ->
        let lines = String.split ~by:"\n" event_str in
        let data_lines = ref [] in
        let event_type = ref None in
        let id = ref None in
        List.for_each lines ~fn:(
          fun line ->
            let line = String.trim line in
            if line = "" then
              ()
            else
              if String.starts_with ~prefix:"data: " line then
                data_lines := String.sub line ~offset:6 ~len:(String.length line - 6) :: !data_lines
              else
                if String.starts_with ~prefix:"data:" line then
                  data_lines := String.sub line ~offset:5 ~len:(String.length line - 5) :: !data_lines
                else
                  if String.starts_with ~prefix:"event: " line then
                    event_type := Some (String.sub line ~offset:7 ~len:(String.length line - 7))
                  else
                    if String.starts_with ~prefix:"id: " line then
                      id := Some (String.sub line ~offset:4 ~len:(String.length line - 4))
                    else
                      if String.starts_with ~prefix:":" line then
                        ()
                      (* Comment line, ignore *)
                      else ()
        );
        let data = String.concat "\n" (List.reverse !data_lines) in
        (* Check for [DONE] marker *)
        if data = "[DONE]" then
          Some (None, remaining)
        else
          let event = { data; event_type = !event_type; id = !id } in Some (Some event, remaining)

  let rec next = fun state ->
    if state.done_ then
      None
    else
      (* Try to parse an event from current buffer *)
      match parse_event state.buffer with
      | Some (Some event, remaining) ->
          state.buffer <- remaining;
          Some event
      | Some (None, remaining) ->
          (* Got [DONE] marker or empty event *)
          state.buffer <- remaining;
          state.done_ <- true;
          None
      | None -> (
        (* Need more data from connection - this will block until data arrives *)
        match Connection.stream state.conn with
        | Error _e ->
            state.done_ <- true;
            None
        | Ok msgs ->
            (* Accumulate data messages, ignore status/headers *)
            List.for_each msgs ~fn:(
              fun msg ->
                match msg with
                | Connection.Data chunk -> state.buffer <- state.buffer ^ chunk
                | Connection.Done -> state.done_ <- true
                | Connection.Status _ | Connection.Headers _ -> ()
            );
            if state.done_ && state.buffer = "" then
              None
            else (* Try parsing again with new data *)
            next state
      )

  let size = fun _state -> 0

  (* Unknown size for streaming *)
  let clone = fun state -> { conn = state.conn; buffer = state.buffer; done_ = state.done_ }
end

let await = fun conn ->
  let module I = SSEIterator in
  Iter.MutIterator.make (module I) { I.conn; buffer = ""; done_ = false }
