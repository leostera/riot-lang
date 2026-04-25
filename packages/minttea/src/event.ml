open Std

type modifier =
  | NoModifier
  | Ctrl
  | Alt
  | Shift
  | CtrlAlt
  | CtrlShift
  | AltShift
  | CtrlAltShift

type key =
  | Up
  | Down
  | Left
  | Right
  | Space
  | Escape
  | Backspace
  | Enter
  | Tab
  | Delete
  | Insert
  | Home
  | End
  | PageUp
  | PageDown
  | F of int
  | Key of string

let key_to_string = fun key ->
  match key with
  | Up -> "up"
  | Down -> "down"
  | Left -> "left"
  | Right -> "right"
  | Space -> "space"
  | Escape -> "esc"
  | Backspace -> "backspace"
  | Enter -> "enter"
  | Tab -> "tab"
  | Delete -> "delete"
  | Insert -> "insert"
  | Home -> "home"
  | End -> "end"
  | PageUp -> "pgup"
  | PageDown -> "pgdn"
  | F n -> "f" ^ Int.to_string n
  | Key key -> key

let modifier_to_string = function
  | NoModifier -> ""
  | Ctrl -> "ctrl"
  | Alt -> "alt"
  | Shift -> "shift"
  | CtrlAlt -> "ctrl+alt"
  | CtrlShift -> "ctrl+shift"
  | AltShift -> "alt+shift"
  | CtrlAltShift -> "ctrl+alt+shift"

type mouse_button =
  | Left
  | Middle
  | Right
  | WheelUp
  | WheelDown

type mouse_event_type =
  | Click
  | Release
  | Motion

type mouse_event = {
  button: mouse_button;
  event_type: mouse_event_type;
  x: int;
  y: int;
  ctrl: bool;
  alt: bool;
  shift: bool;
}

type window_size = { width: int; height: int }

type t =
  | KeyDown of key * modifier
  | Mouse of mouse_event
  | Resize of window_size
  | Timer of Timer.id Ref.t
  | Frame of Time.Instant.t
  | Paste of string
  | FocusGained
  | FocusLost
  | Custom of Message.t

let to_string = function
  | KeyDown (key, mod_) ->
      let mod_str = modifier_to_string mod_ in
      let key_str = key_to_string key in
      if mod_str = "" then
        "KeyDown(" ^ key_str ^ ")"
      else "KeyDown(" ^ mod_str ^ "+" ^ key_str ^ ")"
  | Mouse { button; event_type; x; y; _ } ->
      let btn =
        match button with
        | Left -> "left"
        | Middle -> "middle"
        | Right -> "right"
        | WheelUp -> "wheel_up"
        | WheelDown -> "wheel_down"
      in
      let evt =
        match event_type with
        | Click -> "click"
        | Release -> "release"
        | Motion -> "motion"
      in
      "Mouse(" ^ btn ^ "," ^ evt ^ ",x=" ^ Int.to_string x ^ ",y=" ^ Int.to_string y ^ ")"
  | Resize { width; height } -> "Resize(w=" ^ Int.to_string width ^ ",h=" ^ Int.to_string height ^ ")"
  | Timer _ref -> "Timer(...)"
  | Frame _instant -> "Frame(...)"
  | Paste content ->
      let preview =
        if String.length content > 20 then
          String.sub content ~offset:0 ~len:17 ^ "..."
        else content
      in
      "Paste(" ^ preview ^ ")"
  | FocusGained -> "FocusGained"
  | FocusLost -> "FocusLost"
  | Custom _msg -> "Custom"
