open Std
open Std.Sync
open Std.Sync.Cell

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

type modifier = Shift | Alt | Ctrl | Meta | Super | Hyper

type key_event_kind = Press | Release | Repeat

type mouse_button = Left | Middle | Right | ScrollUp | ScrollDown | ScrollLeft | ScrollRight
type mouse_action = Mouse_press | Mouse_release | Mouse_drag | Mouse_move

type key_event = {
  code : key;
  modifiers : modifier list;
  kind : key_event_kind;
}

type mouse_event = {
  button : mouse_button;
  action : mouse_action;
  x : int;
  y : int;
  modifiers : modifier list;
}

type event =
  [ `Key of key_event
  | `Mouse of mouse_event
  | `Resize of int * int
  | `Paste of string
  | `FocusGained
  | `FocusLost
  | `Unknown of string
  | `Retry
  | `End
  ]

let key_to_string = function
  | Char c -> String.make 1 c
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
  | Media mk -> (
      match mk with
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
      | MuteVolume -> "media-mutevolume")

let modifier_to_string = function
  | Shift -> "shift"
  | Alt -> "alt"
  | Ctrl -> "ctrl"
  | Meta -> "meta"
  | Super -> "super"
  | Hyper -> "hyper"

let button_to_string = function
  | Left -> "left"
  | Middle -> "middle"
  | Right -> "right"
  | ScrollUp -> "scroll-up"
  | ScrollDown -> "scroll-down"
  | ScrollLeft -> "scroll-left"
  | ScrollRight -> "scroll-right"

let event_to_string = function
  | `Key { code; modifiers; kind } ->
      let key_str = key_to_string code in
      let mod_str = String.concat "+" (List.map modifier_to_string modifiers) in
      let kind_str =
        match kind with Press -> "" | Release -> ":release" | Repeat -> ":repeat"
      in
      let base = if mod_str = "" then key_str else mod_str ^ "+" ^ key_str in
      base ^ kind_str
  | `Mouse { button; action; x; y; modifiers } ->
      let act =
        match action with
        | Mouse_press -> "press"
        | Mouse_release -> "release"
        | Mouse_drag -> "drag"
        | Mouse_move -> "move"
      in
      "mouse(" ^ button_to_string button ^ "," ^ act ^ "," ^ Int.to_string x ^ "," ^ Int.to_string y ^ ")"
  | `Resize (w, h) -> "resize(" ^ Int.to_string w ^ "," ^ Int.to_string h ^ ")"
  | `Paste s ->
      let preview =
        if String.length s > 20 then String.sub s 0 17 ^ "..." else s
      in
      "paste(\"" ^ String.escaped preview ^ "\")"
  | `FocusGained -> "focus-gained"
  | `FocusLost -> "focus-lost"
  | `Unknown s -> "unknown(\"" ^ String.escaped s ^ "\")"
  | `Retry -> "retry"
  | `End -> "end"

let pp_event _fmt _event = ()

(* Helper to create key event *)
let make_key ?(kind = Press) ?(mods = []) code =
  `Key { code; modifiers = mods; kind }

(* Parse CSI sequence like \x1b[A for Up arrow *)
let parse_csi seq =
  let len = String.length seq in
  if len < 3 then None
  else if seq.[0] = '\x1b' && seq.[1] = '[' then
    let rest = String.sub seq 2 (len - 2) in
    match rest with
    (* Simple cursor keys *)
    | "A" -> Some (make_key Up)
    | "B" -> Some (make_key Down)
    | "C" -> Some (make_key Right)
    | "D" -> Some (make_key Left)
    | "H" -> Some (make_key Home)
    | "F" -> Some (make_key End)
    | "Z" -> Some (make_key ~mods:[Shift] BackTab)  (* Shift+Tab *)
    (* Extended sequences *)
    | "1~" | "7~" -> Some (make_key Home)
    | "4~" | "8~" -> Some (make_key End)
    | "2~" -> Some (make_key Insert)
    | "3~" -> Some (make_key Delete)
    | "5~" -> Some (make_key PageUp)
    | "6~" -> Some (make_key PageDown)
    (* Function keys *)
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
    (* Modified keys - parse modifier parameter *)
    | _ when String.contains rest ';' -> (
        (* Format: \x1b[1;NX where N is modifier and X is key *)
        match String.split_on_char ';' rest with
        | [ _; mod_key ] when String.length mod_key >= 2 -> (
            let mod_num =
              int_of_string (String.sub mod_key 0 (String.length mod_key - 1))
            in
            let key_char = mod_key.[String.length mod_key - 1] in
            let mods =
              let base_mods =
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
              base_mods
            in
            match key_char with
            | 'A' -> Some (make_key ~mods Up)
            | 'B' -> Some (make_key ~mods Down)
            | 'C' -> Some (make_key ~mods Right)
            | 'D' -> Some (make_key ~mods Left)
            | 'H' -> Some (make_key ~mods Home)
            | 'F' -> Some (make_key ~mods End)
            | _ -> None)
        | _ -> None)
    (* Mouse sequences - SGR mode \x1b[<Cb;Cx;Cy(M|m) *)
    | _ when String.length rest > 0 && rest.[0] = '<' -> (
        try
          let mouse_data = String.sub rest 1 (String.length rest - 1) in
          let last_char = mouse_data.[String.length mouse_data - 1] in
          let is_release = last_char = 'm' in
          let coords =
            String.sub mouse_data 0 (String.length mouse_data - 1)
          in
          match String.split_on_char ';' coords with
          | [ cb; cx; cy ] ->
              let b = int_of_string cb in
              let x = int_of_string cx in
              let y = int_of_string cy in
              let button_code = b land 0x43 in
              (* Mask for button *)
              let button =
                match button_code with
                | 0 -> Left
                | 1 -> Middle
                | 2 -> Right
                | 64 -> ScrollUp
                | 65 -> ScrollDown
                | _ -> Left
              in
              let has_shift = b land 4 != 0 in
              let has_meta = b land 8 != 0 in
              let has_ctrl = b land 16 != 0 in
              let is_motion = b land 32 != 0 in
              let modifiers =
                []
                |> (fun acc -> if has_shift then Shift :: acc else acc)
                |> (fun acc -> if has_meta then Meta :: acc else acc)
                |> (fun acc -> if has_ctrl then Ctrl :: acc else acc)
              in
              let action =
                if is_release then Mouse_release
                else if is_motion then if button_code = 3 then Mouse_move else Mouse_drag
                else Mouse_press
              in
              Some
                (`Mouse { button; action; x; y; modifiers = List.rev modifiers })
          | _ -> None
        with _ -> None)
    (* Focus events *)
    | "I" -> Some `FocusGained
    | "O" -> Some `FocusLost
    | _ -> None
  else None

(* Parse SS3 sequence like \x1bOA for arrow keys in some modes *)
let parse_ss3 seq =
  let len = String.length seq in
  if len = 3 && seq.[0] = '\x1b' && seq.[1] = 'O' then
    match seq.[2] with
    | 'A' -> Some (make_key Up)
    | 'B' -> Some (make_key Down)
    | 'C' -> Some (make_key Right)
    | 'D' -> Some (make_key Left)
    | 'H' -> Some (make_key Home)
    | 'F' -> Some (make_key End)
    | 'P' -> Some (make_key (F 1))
    | 'Q' -> Some (make_key (F 2))
    | 'R' -> Some (make_key (F 3))
    | 'S' -> Some (make_key (F 4))
    | _ -> None
  else None

(* Parse OSC sequence for bracketed paste *)
let parse_osc seq =
  if String.length seq >= 6 then
    (* Bracketed paste start: \x1b[200~ *)
    if String.sub seq 0 6 = "\x1b[200~" then
      (* Read until end marker \x1b[201~ *)
      (* This is tricky in non-blocking mode, so for now return unknown *)
      (* A real implementation would buffer until end marker *)
      Some (`Unknown seq)
    else None
  else None

let parse_escape seq =
  if String.length seq = 0 then None
  else
    match seq.[0] with
    | '\x1b' when String.length seq > 1 -> (
        match seq.[1] with
        | '[' -> parse_csi seq
        | 'O' -> parse_ss3 seq
        | _ -> parse_osc seq)
    | _ -> None

(* Buffer for accumulating escape sequences *)
let escape_buffer = Cell.create ""
let in_paste = Cell.create false
let paste_buffer = Cell.create ""

let read_event () =
  match Stdin.read_utf8 () with
  | `End -> `End
  | `Retry -> `Retry
  | `Malformed reason -> `Unknown ("malformed: " ^ reason)
  | `Read s -> (
      (* Check for bracketed paste markers *)
      if s = "\x1b[200~" then (
        Cell.set in_paste true;
        Cell.set paste_buffer "";
        `Retry)
      else if s = "\x1b[201~" then (
        Cell.set in_paste false;
        let content = Cell.get paste_buffer in
        Cell.set paste_buffer "";
        `Paste content)
      else if Cell.get in_paste then (
        Cell.set paste_buffer (Cell.get paste_buffer ^ s);
        `Retry)
      else if String.length s = 1 then
        match s.[0] with
        (* Control characters *)
        | '\r' | '\n' -> make_key Enter
        | '\t' -> make_key Tab
        | '\x7f' -> make_key Backspace
        | '\x1b' ->
            (* Start of escape sequence, buffer it *)
            Cell.set escape_buffer s;
            `Retry
        | ' ' -> make_key Space
        (* Ctrl+letter combinations *)
        | '\x01' -> make_key ~mods:[Ctrl] (Char 'a')
        | '\x02' -> make_key ~mods:[Ctrl] (Char 'b')
        | '\x03' -> make_key ~mods:[Ctrl] (Char 'c')
        | '\x04' -> make_key ~mods:[Ctrl] (Char 'd')
        | '\x05' -> make_key ~mods:[Ctrl] (Char 'e')
        | '\x06' -> make_key ~mods:[Ctrl] (Char 'f')
        | '\x07' -> make_key ~mods:[Ctrl] (Char 'g')
        | '\x08' -> make_key Backspace
        | '\x0b' -> make_key ~mods:[Ctrl] (Char 'k')
        | '\x0c' -> make_key ~mods:[Ctrl] (Char 'l')
        | '\x0e' -> make_key ~mods:[Ctrl] (Char 'n')
        | '\x0f' -> make_key ~mods:[Ctrl] (Char 'o')
        | '\x10' -> make_key ~mods:[Ctrl] (Char 'p')
        | '\x11' -> make_key ~mods:[Ctrl] (Char 'q')
        | '\x12' -> make_key ~mods:[Ctrl] (Char 'r')
        | '\x13' -> make_key ~mods:[Ctrl] (Char 's')
        | '\x14' -> make_key ~mods:[Ctrl] (Char 't')
        | '\x15' -> make_key ~mods:[Ctrl] (Char 'u')
        | '\x16' -> make_key ~mods:[Ctrl] (Char 'v')
        | '\x17' -> make_key ~mods:[Ctrl] (Char 'w')
        | '\x18' -> make_key ~mods:[Ctrl] (Char 'x')
        | '\x19' -> make_key ~mods:[Ctrl] (Char 'y')
        | '\x1a' -> make_key ~mods:[Ctrl] (Char 'z')
        | c -> make_key (Char c)
      else if Cell.get escape_buffer != "" then (
        (* Continue building escape sequence *)
        Cell.set escape_buffer (Cell.get escape_buffer ^ s);
        match parse_escape (Cell.get escape_buffer) with
        | Some event ->
            Cell.set escape_buffer "";
            event
        | None ->
            (* Keep buffering or timeout *)
            if String.length (Cell.get escape_buffer) > 20 then (
              let unknown = Cell.get escape_buffer in
              Cell.set escape_buffer "";
              `Unknown unknown)
            else `Retry)
      else
        (* Multi-char string, try to parse as escape *)
        match parse_escape s with
        | Some event -> event
        | None -> `Unknown s)

(* Non-blocking try_read *)
let try_read () =
  match read_event () with
  | `Retry | `End -> None
  | event -> Some event
