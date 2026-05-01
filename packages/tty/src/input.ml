open Std

type media_key =
  | Play
  | Pause_media
  | PlayPause
  | Stop
  | FastForward
  | Rewind
  | TrackNext
  | TrackPrevious
  | Record
  | LowerVolume
  | RaiseVolume
  | MuteVolume

type key =
  | Char of char
  | Enter
  | Tab
  | BackTab
  | Backspace
  | Escape
  | Space
  | Up
  | Down
  | Left
  | Right
  | Home
  | End
  | PageUp
  | PageDown
  | Insert
  | Delete
  | F of int
  | CapsLock
  | ScrollLock
  | NumLock
  | PrintScreen
  | Pause
  | Menu
  | KeypadBegin
  | Media of media_key

type modifier =
  | Shift
  | Alt
  | Ctrl
  | Meta
  | Super
  | Hyper

type key_event_kind =
  | Press
  | Release
  | Repeat

type mouse_button =
  | Left
  | Middle
  | Right
  | ScrollUp
  | ScrollDown
  | ScrollLeft
  | ScrollRight

type mouse_action =
  | Mouse_press
  | Mouse_release
  | Mouse_drag
  | Mouse_move

type key_event = {
  code: key;
  modifiers: modifier list;
  kind: key_event_kind;
}

type mouse_event = {
  button: mouse_button;
  action: mouse_action;
  x: int;
  y: int;
  modifiers: modifier list;
}

type event = [
  | `Key of key_event
  | `Text of string
  | `Mouse of mouse_event
  | `Resize of int * int
  | `Paste of string
  | `FocusGained
  | `FocusLost
  | `Unknown of string
  | `Retry
  | `End
]

let string_of_char = fun char -> String.make ~len:1 ~char

let key_to_string = fun __tmp1 ->
  match __tmp1 with
  | Char c -> string_of_char c
  | Enter -> "enter"
  | Tab -> "tab"
  | BackTab -> "backtab"
  | Backspace -> "backspace"
  | Escape -> "escape"
  | Space -> "space"
  | Up -> "up"
  | Down -> "down"
  | Left -> "left"
  | Right -> "right"
  | Home -> "home"
  | End -> "end"
  | PageUp -> "pageup"
  | PageDown -> "pagedown"
  | Insert -> "insert"
  | Delete -> "delete"
  | F n -> "f" ^ Int.to_string n
  | CapsLock -> "capslock"
  | ScrollLock -> "scrolllock"
  | NumLock -> "numlock"
  | PrintScreen -> "printscreen"
  | Pause -> "pause"
  | Menu -> "menu"
  | KeypadBegin -> "keypadbegin"
  | Media key -> (
      match key with
      | Play -> "media-play"
      | Pause_media -> "media-pause"
      | PlayPause -> "media-playpause"
      | Stop -> "media-stop"
      | FastForward -> "media-fastforward"
      | Rewind -> "media-rewind"
      | TrackNext -> "media-tracknext"
      | TrackPrevious -> "media-trackprevious"
      | Record -> "media-record"
      | LowerVolume -> "media-lowervolume"
      | RaiseVolume -> "media-raisevolume"
      | MuteVolume -> "media-mutevolume"
    )

let modifier_to_string = fun __tmp1 ->
  match __tmp1 with
  | Shift -> "shift"
  | Alt -> "alt"
  | Ctrl -> "ctrl"
  | Meta -> "meta"
  | Super -> "super"
  | Hyper -> "hyper"

let button_to_string = fun __tmp1 ->
  match __tmp1 with
  | Left -> "left"
  | Middle -> "middle"
  | Right -> "right"
  | ScrollUp -> "scroll-up"
  | ScrollDown -> "scroll-down"
  | ScrollLeft -> "scroll-left"
  | ScrollRight -> "scroll-right"

let event_to_string = fun __tmp1 ->
  match __tmp1 with
  | `Key { code; modifiers; kind } ->
      let key_str = key_to_string code in
      let mod_str = String.concat "+" (List.map modifiers ~fn:modifier_to_string) in
      let kind_str =
        match kind with
        | Press -> ""
        | Release -> ":release"
        | Repeat -> ":repeat"
      in
      let base =
        if mod_str = "" then
          key_str
        else
          mod_str ^ "+" ^ key_str
      in
      base ^ kind_str
  | `Text value -> "text(\"" ^ String.escaped value ^ "\")"
  | `Mouse {
    button;
    action;
    x;
    y;
    modifiers = _;
  } ->
      let act =
        match action with
        | Mouse_press -> "press"
        | Mouse_release -> "release"
        | Mouse_drag -> "drag"
        | Mouse_move -> "move"
      in
      "mouse("
      ^ button_to_string button
      ^ ","
      ^ act
      ^ ","
      ^ Int.to_string x
      ^ ","
      ^ Int.to_string y
      ^ ")"
  | `Resize (w, h) -> "resize(" ^ Int.to_string w ^ "," ^ Int.to_string h ^ ")"
  | `Paste s ->
      let preview =
        if String.length s > 20 then
          String.sub s ~offset:0 ~len:17 ^ "..."
        else
          s
      in
      "paste(\"" ^ String.escaped preview ^ "\")"
  | `FocusGained -> "focus-gained"
  | `FocusLost -> "focus-lost"
  | `Unknown s -> "unknown(\"" ^ String.escaped s ^ "\")"
  | `Retry -> "retry"
  | `End -> "end"

let pp_event = fun _fmt _event -> ()

let make_key = fun ?(kind = Press) ?(mods = []) code -> `Key { code; modifiers = mods; kind }

module Token = struct
  type control =
    | Escape
    | Csi of { raw: string; body: string }
    | Ss3 of { raw: string; body: string }
    | Osc of { raw: string; body: string }

  type t =
    | Text of string
    | Control of control
    | Unknown of string
end

module Tokenizer = struct
  type t = { pending: string }

  let create = fun () -> { pending = "" }

  let rec reverse = fun acc ->
    fun __tmp1 ->
      match __tmp1 with
      | [] -> acc
      | item :: rest -> reverse (item :: acc) rest

  let is_csi_final = fun char ->
    let code = Char.to_int char in
    Int.(code >= 0x40 && code <= 0x7e)

  let rec find_next_escape = fun input ~from ->
    if from >= String.length input then
      from
    else if Char.equal (String.get_unchecked input ~at:from) '\x1b' then
      from
    else
      find_next_escape input ~from:(from + 1)

  let rec find_csi_end = fun input ~from ->
    if from >= String.length input then
      None
    else if is_csi_final (String.get_unchecked input ~at:from) then
      Some from
    else
      find_csi_end input ~from:(from + 1)

  let rec find_osc_end = fun input ~from ->
    if from >= String.length input then
      None
    else
      let char = String.get_unchecked input ~at:from in
      if Char.equal char '\x07' then
        Some (from, from + 1)
      else if Char.equal char '\x1b' then
        if
          from + 1 < String.length input
          && Char.equal (String.get_unchecked input ~at:(from + 1)) '\\'
        then
          Some (from, from + 2)
        else
          find_osc_end input ~from:(from + 1)
      else
        find_osc_end input ~from:(from + 1)

  let rec scan = fun input ~at acc ->
    if at >= String.length input then
      (reverse [] acc, "")
    else
      let char = String.get_unchecked input ~at in
      if not (Char.equal char '\x1b') then
        let next_escape = find_next_escape input ~from:at in
        let text = String.sub input ~offset:at ~len:(next_escape - at) in
        scan input ~at:next_escape (Token.Text text :: acc)
      else if at + 1 >= String.length input then
        (reverse [] acc, String.sub input ~offset:at ~len:(String.length input - at))
      else
        match String.get_unchecked input ~at:(at + 1) with
        | '[' -> (
            match find_csi_end input ~from:(at + 2) with
            | None -> (reverse [] acc, String.sub input ~offset:at ~len:(String.length input - at))
            | Some end_at ->
                let raw = String.sub input ~offset:at ~len:(end_at - at + 1) in
                let body = String.sub input ~offset:(at + 2) ~len:(end_at - at - 1) in
                scan input ~at:(end_at + 1) (Token.Control (Token.Csi { raw; body }) :: acc)
          )
        | 'O' ->
            if at + 2 >= String.length input then
              (reverse [] acc, String.sub input ~offset:at ~len:(String.length input - at))
            else
              let raw = String.sub input ~offset:at ~len:3 in
              let body = String.sub input ~offset:(at + 2) ~len:1 in
              scan input ~at:(at + 3) (Token.Control (Token.Ss3 { raw; body }) :: acc)
        | ']' -> (
            match find_osc_end input ~from:(at + 2) with
            | None -> (reverse [] acc, String.sub input ~offset:at ~len:(String.length input - at))
            | Some (terminator_at, next_at) ->
                let raw = String.sub input ~offset:at ~len:(next_at - at) in
                let body = String.sub input ~offset:(at + 2) ~len:(terminator_at - at - 2) in
                scan input ~at:next_at (Token.Control (Token.Osc { raw; body }) :: acc)
          )
        | _ -> scan input ~at:(at + 1) (Token.Control Token.Escape :: acc)

  let feed = fun state chunk ->
    let input =
      if String.is_empty state.pending then
        chunk
      else
        state.pending ^ chunk
    in
    let (tokens, pending) = scan input ~at:0 [] in
    ({ pending }, tokens)

  let flush = fun state ->
    if String.is_empty state.pending then
      (create (), [])
    else if String.equal state.pending "\x1b" then
      (create (), [ Token.Control Token.Escape ])
    else
      (create (), [ Token.Unknown state.pending ])
end

module Parser = struct
  type t = {
    tokenizer: Tokenizer.t;
    pending_escape: bool;
    in_paste: bool;
    paste_buffer: string;
  }

  let create = fun () ->
    {
      tokenizer = Tokenizer.create ();
      pending_escape = false;
      in_paste = false;
      paste_buffer = "";
    }

  let clear_pending_escape = fun state -> { state with pending_escape = false }

  let append_paste = fun state fragment -> {
    state with
    paste_buffer = state.paste_buffer ^ fragment;
  }

  let raw_of_control = fun __tmp1 ->
    match __tmp1 with
    | Token.Escape -> "\x1b"
    | Token.Csi { raw; body = _ }
    | Token.Ss3 { raw; body = _ }
    | Token.Osc { raw; body = _ } -> raw

  let parse_mouse = fun body ->
    match String.length body with
    | length when Int.(length > 0) && Char.equal (String.get_unchecked body ~at:0) '<' -> (
        let data = String.sub body ~offset:1 ~len:(length - 1) in
        let last_char = String.get_unchecked data ~at:(String.length data - 1) in
        let is_release = Char.equal last_char 'm' in
        let coords = String.sub data ~offset:0 ~len:(String.length data - 1) in
        match String.split ~by:";" coords with
        | [ cb; cx; cy ] -> (
            match (Int.parse cb, Int.parse cx, Int.parse cy) with
            | (Some code, Some x, Some y) ->
                let button_code = code land 0x43 in
                let button =
                  match button_code with
                  | 0 -> Left
                  | 1 -> Middle
                  | 2 -> Right
                  | 64 -> ScrollUp
                  | 65 -> ScrollDown
                  | 66 -> ScrollLeft
                  | 67 -> ScrollRight
                  | _ -> Left
                in
                let modifiers =
                  []
                  |> (fun acc ->
                    if code land 4 != 0 then
                      Shift :: acc
                    else
                      acc)
                  |> (fun acc ->
                    if code land 8 != 0 then
                      Meta :: acc
                    else
                      acc)
                  |> (fun acc ->
                    if code land 16 != 0 then
                      Ctrl :: acc
                    else
                      acc)
                  |> List.reverse
                in
                let is_motion = code land 32 != 0 in
                let action =
                  if is_release then
                    Mouse_release
                  else if is_motion then
                    if button_code = 3 then
                      Mouse_move
                    else
                      Mouse_drag
                  else
                    Mouse_press
                in
                Some (`Mouse {
                  button;
                  action;
                  x;
                  y;
                  modifiers;
                })
            | _ -> None
          )
        | _ -> None
      )
    | _ -> None

  let parse_modified_csi = fun body ->
    match String.split ~by:";" body with
    | [ _; mod_key ] when String.length mod_key >= 2 -> (
        match Int.parse (String.sub mod_key ~offset:0 ~len:(String.length mod_key - 1)) with
        | None -> None
        | Some mod_num ->
            let key_char = String.get_unchecked mod_key ~at:(String.length mod_key - 1) in
            let modifiers =
              match mod_num with
              | 2 -> [ Shift ]
              | 3 -> [ Alt ]
              | 4 -> [ Alt; Shift ]
              | 5 -> [ Ctrl ]
              | 6 -> [ Ctrl; Shift ]
              | 7 -> [ Ctrl; Alt ]
              | 8 -> [ Ctrl; Alt; Shift ]
              | _ -> []
            in
            (
              match key_char with
              | 'A' -> Some (make_key ~mods:modifiers Up)
              | 'B' -> Some (make_key ~mods:modifiers Down)
              | 'C' -> Some (make_key ~mods:modifiers Right)
              | 'D' -> Some (make_key ~mods:modifiers Left)
              | 'H' -> Some (make_key ~mods:modifiers Home)
              | 'F' -> Some (make_key ~mods:modifiers End)
              | _ -> None
            )
      )
    | _ -> None

  let parse_csi = fun body ->
    match body with
    | "A" -> Some (make_key Up)
    | "B" -> Some (make_key Down)
    | "C" -> Some (make_key Right)
    | "D" -> Some (make_key Left)
    | "H" -> Some (make_key Home)
    | "F" -> Some (make_key End)
    | "Z" -> Some (make_key ~mods:[ Shift ] BackTab)
    | "1~"
    | "7~" -> Some (make_key Home)
    | "4~"
    | "8~" -> Some (make_key End)
    | "2~" -> Some (make_key Insert)
    | "3~" -> Some (make_key Delete)
    | "5~" -> Some (make_key PageUp)
    | "6~" -> Some (make_key PageDown)
    | "11~" -> Some (make_key (F 1))
    | "12~" -> Some (make_key (F 2))
    | "13~" -> Some (make_key (F 3))
    | "14~" -> Some (make_key (F 4))
    | "15~" -> Some (make_key (F 5))
    | "17~" -> Some (make_key (F 6))
    | "18~" -> Some (make_key (F 7))
    | "19~" -> Some (make_key (F 8))
    | "20~" -> Some (make_key (F 9))
    | "21~" -> Some (make_key (F 10))
    | "23~" -> Some (make_key (F 11))
    | "24~" -> Some (make_key (F 12))
    | "I" -> Some `FocusGained
    | "O" -> Some `FocusLost
    | _ ->
        if String.starts_with ~prefix:"<" body then
          parse_mouse body
        else if String.contains body ";" then
          parse_modified_csi body
        else
          None

  let parse_ss3 = fun body ->
    match body with
    | "A" -> Some (make_key Up)
    | "B" -> Some (make_key Down)
    | "C" -> Some (make_key Right)
    | "D" -> Some (make_key Left)
    | "H" -> Some (make_key Home)
    | "F" -> Some (make_key End)
    | "P" -> Some (make_key (F 1))
    | "Q" -> Some (make_key (F 2))
    | "R" -> Some (make_key (F 3))
    | "S" -> Some (make_key (F 4))
    | _ -> None

  let event_of_ascii_char = fun ~mods char ->
    let code = Char.code char in
    if Char.equal char '\r' || Char.equal char '\n' then
      make_key ~mods Enter
    else if Char.equal char '\t' then
      make_key ~mods Tab
    else if Char.equal char '\x7f' || Char.equal char '\x08' then
      make_key ~mods Backspace
    else if Char.equal char ' ' then
      make_key ~mods Space
    else if code >= 1 && code <= 26 then
      let ctrl = Char.from_int_unchecked (code + 96) in
      make_key ~mods:(mods @ [ Ctrl ]) (Char ctrl)
    else if Char.equal char '\x1b' then
      make_key ~mods Escape
    else if code < 32 then
      `Unknown (string_of_char char)
    else
      make_key ~mods (Char char)

  let split_first_rune = fun text ->
    match Unicode.Utf8.decode_rune text 0 with
    | Some (_rune, len) ->
        let head = String.sub text ~offset:0 ~len in
        let rest = String.sub text ~offset:len ~len:(String.length text - len) in
        (head, rest)
    | None ->
        let head = String.sub text ~offset:0 ~len:1 in
        let rest = String.sub text ~offset:1 ~len:(String.length text - 1) in
        (head, rest)

  let rec events_of_text = fun ?(mods = []) text acc ->
    if String.is_empty text then
      Tokenizer.reverse [] acc
    else
      let (head, rest) = split_first_rune text in
      if String.length head = 1 then
        let char = String.get_unchecked head ~at:0 in
        events_of_text ~mods rest (event_of_ascii_char ~mods char :: acc)
      else
        events_of_text ~mods rest ((`Text head) :: acc)

  let handle_text = fun state text ->
    if state.in_paste then
      (append_paste state text, [])
    else if state.pending_escape then
      let (head, rest) = split_first_rune text in
      let first_events =
        if String.length head = 1 then
          let char = String.get_unchecked head ~at:0 in
          [ event_of_ascii_char ~mods:[ Alt ] char ]
        else
          [
            `Unknown ("\x1b" ^ head);
          ]
      in
      let state = clear_pending_escape state in
      let rest_events = events_of_text rest [] in
      (state, first_events @ rest_events)
    else
      (state, events_of_text text [])

  let handle_control = fun state ->
    fun __tmp1 ->
      match __tmp1 with
      | Token.Escape ->
          if state.in_paste then
            (append_paste state "\x1b", [])
          else if state.pending_escape then
            ({ state with pending_escape = true }, [ make_key Escape ])
          else
            ({ state with pending_escape = true }, [])
      | control -> (
          let raw = raw_of_control control in
          match control with
          | Token.Csi { raw = _; body } ->
              if state.in_paste && not (String.equal body "201~") then
                (append_paste state raw, [])
              else if String.equal body "200~" then
                ({ (clear_pending_escape state) with in_paste = true; paste_buffer = "" }, [])
              else if String.equal body "201~" then
                let event = `Paste state.paste_buffer in
                (
                  { (clear_pending_escape state) with in_paste = false; paste_buffer = "" },
                  [ event ]
                )
              else
                let state = clear_pending_escape state in
                (
                  match parse_csi body with
                  | Some event -> (state, [ event ])
                  | None -> (state, [
                    `Unknown raw;
                  ])
                )
          | Token.Ss3 { raw = _; body } ->
              if state.in_paste then
                (append_paste state raw, [])
              else
                let state = clear_pending_escape state in
                (
                  match parse_ss3 body with
                  | Some event -> (state, [ event ])
                  | None -> (state, [
                    `Unknown raw;
                  ])
                )
          | Token.Osc { raw = _; body = _ } ->
              if state.in_paste then
                (append_paste state raw, [])
              else
                (clear_pending_escape state, [
                  `Unknown raw;
                ])
          | Token.Escape -> (state, [])
        )

  let handle_unknown = fun state raw ->
    if state.in_paste then
      (append_paste state raw, [])
    else
      (clear_pending_escape state, [
        `Unknown raw;
      ])

  let rec handle_tokens = fun state tokens acc ->
    match tokens with
    | [] -> (state, Tokenizer.reverse [] acc)
    | token :: rest ->
        let (state, events) =
          match token with
          | Token.Text text -> handle_text state text
          | Token.Control control -> handle_control state control
          | Token.Unknown raw -> handle_unknown state raw
        in
        handle_tokens
          state
          rest
          (Tokenizer.reverse acc events)

  let flush_pending = fun state ->
    let events = [] in
    let (state, events) =
      if state.pending_escape then
        ({ state with pending_escape = false }, make_key Escape :: events)
      else
        (state, events)
    in
    let (state, events) =
      if state.in_paste then
        ({ state with in_paste = false; paste_buffer = "" }, (`Paste state.paste_buffer) :: events)
      else
        (state, events)
    in
    (state, Tokenizer.reverse [] events)

  let feed = fun state chunk ->
    let (tokenizer, tokens) = Tokenizer.feed state.tokenizer chunk in
    let state = { state with tokenizer } in
    handle_tokens state tokens []

  let flush = fun state ->
    let (tokenizer, tokens) = Tokenizer.flush state.tokenizer in
    let state = { state with tokenizer } in
    let (state, events) = handle_tokens state tokens [] in
    let (state, flushed) = flush_pending state in
    (state, events @ flushed)
end

let default_parser = ref (Parser.create ())

let queued_events = ref []

let enqueue = fun events -> queued_events := events

let dequeue = fun () ->
  match !queued_events with
  | [] -> None
  | event :: rest ->
      queued_events := rest;
      Some event

let parse_escape = fun seq ->
  let (parser, events) = Parser.feed (Parser.create ()) seq in
  let (_, flushed) = Parser.flush parser in
  match events @ flushed with
  | [ event ] -> Some event
  | _ -> None

let emit_or_retry = fun events ->
  match events with
  | [] -> `Retry
  | event :: rest ->
      enqueue rest;
      event

let read_event = fun () ->
  match dequeue () with
  | Some event -> event
  | None -> (
      match Stdin.read_utf8 () with
      | `End ->
          let (parser, events) = Parser.flush !default_parser in
          default_parser := parser;
          if events = [] then
            `End
          else
            emit_or_retry events
      | `Retry -> `Retry
      | `Malformed reason -> `Unknown ("malformed: " ^ reason)
      | `Read text ->
          let (parser, events) = Parser.feed !default_parser text in
          default_parser := parser;
          emit_or_retry events
    )

let try_read = fun () ->
  match read_event () with
  | `Retry
  | `End -> None
  | event -> Some event
