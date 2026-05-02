open Std
open Std.IO
open Event

type Message.t +=
  | BracketedPasteStart
  | BracketedPasteEnd
  | CursorPosition of { row: int; col: int }
  | WindowTitleChange of string

(**
   ANSI Parser State Machine

   Handles parsing of complex ANSI escape sequences including:
   - Keyboard events with modifiers
   - Mouse events (SGR protocol)
   - Bracketed paste
   - Focus events
   - Window resize
*)
type parser_state =
  | Ground
  | Escape
  | CsiEntry
  | CsiParam
  | CsiIntermediate
  | OscString
  | DcsEntry
  | DcsParam
  | DcsIntermediate
  | DcsPassthrough

type parser = {
  mutable state: parser_state;
  mutable params: int list;
  mutable intermediate: string;
  mutable final_char: char option;
  mutable osc_string: Buffer.t;
  mutable current_param: int;
}

let create = fun () ->
  {
    state = Ground;
    params = [];
    intermediate = "";
    final_char = None;
    osc_string = Buffer.create ~size:256;
    current_param = 0;
  }

let reset = fun p ->
  p.state <- Ground;
  p.params <- [];
  p.intermediate <- "";
  p.final_char <- None;
  Buffer.clear p.osc_string;
  p.current_param <- 0

(* Parse a CSI parameter byte *)

let parse_param = fun p c ->
  match c with
  | '0' .. '9' ->
      let digit = Char.code c - Char.code '0' in
      p.current_param <- p.current_param * 10 + digit
  | ';'
  | ':' ->
      p.params <- p.params @ [ p.current_param ];
      p.current_param <- 0
  | _ -> ()

(* Finalize CSI parameters *)

let finalize_params = fun p ->
  if p.current_param > 0 || List.length p.params > 0 then
    p.params <- p.params @ [ p.current_param ];
  p.current_param <- 0

(* Parse mouse event from SGR protocol *)

let parse_mouse_sgr = fun params ->
  match params with
  | [ button; x; y ] ->
      let button_type =
        match button land 0x3 with
        | 0 -> Event.Left
        | 1 -> Event.Middle
        | 2 -> Event.Right
        | _ -> Event.Left
      in
      let event_type =
        if button land 0x20 != 0 then
          Event.Motion
        else if button land 0x40 != 0 then
          Event.Release
        else
          Event.Click
      in
      let ctrl = button land 0x10 != 0 in
      let alt = button land 0x8 != 0 in
      let shift = button land 0x4 != 0 in
      Some (
        Event.Mouse {
          x = x - 1;
          y = y - 1;
          button = button_type;
          event_type;
          ctrl;
          alt;
          shift;
        }
      )
  | _ -> None

(* Convert CSI sequence to event *)

let csi_to_event = fun p ->
  finalize_params p;
  match p.final_char with
  | Some 'A' -> Some (KeyDown (Up, NoModifier))
  | Some 'B' -> Some (KeyDown (Down, NoModifier))
  | Some 'C' -> Some (KeyDown (Right, NoModifier))
  | Some 'D' -> Some (KeyDown (Left, NoModifier))
  | Some 'H' -> Some (KeyDown (Home, NoModifier))
  | Some 'F' -> Some (KeyDown (End, NoModifier))
  | Some '~' -> (
      match p.params with
      | [ 1 ]
      | [ 7 ] -> Some (KeyDown (Home, NoModifier))
      | [ 2 ] -> Some (KeyDown (Insert, NoModifier))
      | [ 3 ] -> Some (KeyDown (Delete, NoModifier))
      | [ 4 ]
      | [ 8 ] -> Some (KeyDown (End, NoModifier))
      | [ 5 ] -> Some (KeyDown (PageUp, NoModifier))
      | [ 6 ] -> Some (KeyDown (PageDown, NoModifier))
      | [ 11 ] -> Some (KeyDown (F 1, NoModifier))
      | [ 12 ] -> Some (KeyDown (F 2, NoModifier))
      | [ 13 ] -> Some (KeyDown (F 3, NoModifier))
      | [ 14 ] -> Some (KeyDown (F 4, NoModifier))
      | [ 15 ] -> Some (KeyDown (F 5, NoModifier))
      | [ 17 ] -> Some (KeyDown (F 6, NoModifier))
      | [ 18 ] -> Some (KeyDown (F 7, NoModifier))
      | [ 19 ] -> Some (KeyDown (F 8, NoModifier))
      | [ 20 ] -> Some (KeyDown (F 9, NoModifier))
      | [ 21 ] -> Some (KeyDown (F 10, NoModifier))
      | [ 23 ] -> Some (KeyDown (F 11, NoModifier))
      | [ 24 ] -> Some (KeyDown (F 12, NoModifier))
      | [ 200 ] -> Some (Event.Custom BracketedPasteStart)
      | [ 201 ] -> Some (Event.Custom BracketedPasteEnd)
      | _ -> None
    )
  | Some 'M'
  | Some 'm' when String.length p.intermediate = 1 && String.get p.intermediate ~at:0 = Some '<' ->
      (* SGR mouse protocol *)
      parse_mouse_sgr p.params
  | Some 'I' -> Some Event.FocusGained
  | Some 'O' -> Some Event.FocusLost
  | Some 'R' -> (
      match p.params with
      | [ row; col ] -> Some (Event.Custom (CursorPosition { row; col }))
      | _ -> None
    )
  | _ -> None

(* Main parsing function *)

let parse_byte = fun p byte ->
  let c = Char.from_int_unchecked byte in
  match (p.state, c) with
  | (Ground, '\027') ->
      p.state <- Escape;
      None
  | (Escape, '[') ->
      p.state <- CsiEntry;
      None
  | (Escape, ']') ->
      p.state <- OscString;
      None
  | (Escape, 'P') ->
      p.state <- DcsEntry;
      None
  | (Escape, _) ->
      reset p;
      None
  | (CsiEntry, '<')
  | (CsiEntry, '>')
  | (CsiEntry, '?') ->
      p.intermediate <- String.make ~len:1 ~char:c;
      p.state <- CsiParam;
      None
  | (CsiEntry, ('0' .. '9' | ';' | ':')) ->
      parse_param p c;
      p.state <- CsiParam;
      None
  | (CsiEntry, (' ' .. '/' | '<' .. '?')) ->
      p.intermediate <- String.make ~len:1 ~char:c;
      p.state <- CsiIntermediate;
      None
  | (CsiEntry, ('@' .. '~')) ->
      p.final_char <- Some c;
      let event = csi_to_event p in
      reset p;
      event
  | (CsiEntry, _) ->
      reset p;
      None
  | (CsiParam, ('0' .. '9' | ';' | ':')) ->
      parse_param p c;
      None
  | (CsiParam, (' ' .. '/' | '<' .. '?')) ->
      p.intermediate <- p.intermediate ^ String.make ~len:1 ~char:c;
      p.state <- CsiIntermediate;
      None
  | (CsiParam, ('@' .. '~')) ->
      p.final_char <- Some c;
      let event = csi_to_event p in
      reset p;
      event
  | (CsiParam, _) ->
      reset p;
      None
  | (CsiIntermediate, (' ' .. '/' | '<' .. '?')) ->
      p.intermediate <- p.intermediate ^ String.make ~len:1 ~char:c;
      None
  | (CsiIntermediate, ('@' .. '~')) ->
      p.final_char <- Some c;
      let event = csi_to_event p in
      reset p;
      event
  | (CsiIntermediate, _) ->
      reset p;
      None
  | (OscString, '\007')
  | (OscString, '\027') ->
      (* OSC terminated by BEL or ESC *)
      let str = Buffer.contents p.osc_string in
      reset p;
      (* Parse OSC commands *)
      if String.starts_with ~prefix:"2;" str && String.length str > 2 then
        Some (Event.Custom (WindowTitleChange (String.sub str ~offset:2 ~len:(String.length str - 2))))
      else
        None
  | (OscString, c) ->
      Buffer.add_char p.osc_string c;
      None
  | _ ->
      reset p;
      None

(* Parse a string of bytes *)

let parse_string = fun p str ->
  let events = ref [] in
  String.iter
    (fun c ->
      match parse_byte p (Char.code c) with
      | Some event -> events := event :: !events
      | None -> ())
    str;
  List.rev !events

(* Parse normal character input *)

let parse_char = fun c ->
  match c with
  | ' ' -> Event.Space
  | '\027' -> Event.Escape
  | '\127' -> Event.Backspace
  | '\n'
  | '\r' -> Event.Enter
  | '\t' -> Event.Tab
  | c when Char.code c >= 1 && Char.code c <= 26 ->
      (* Ctrl+A through Ctrl+Z *)
      let letter = Char.from_int_unchecked (Char.code c + 96) in
      Event.Key (String.make ~len:1 ~char:letter)
  | c -> Event.Key (String.make ~len:1 ~char:c)
