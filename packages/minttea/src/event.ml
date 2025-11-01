open Std

type modifier =
  | No_modifier
  | Ctrl
  | Alt
  | Shift
  | Ctrl_alt
  | Ctrl_shift
  | Alt_shift
  | Ctrl_alt_shift

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
  | Page_up
  | Page_down
  | F of int
  | Key of string

let key_to_string key =
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
  | Page_up -> "pgup"
  | Page_down -> "pgdn"
  | F n -> Format.sprintf "f%d" n
  | Key key -> key

let modifier_to_string = function
  | No_modifier -> ""
  | Ctrl -> "ctrl"
  | Alt -> "alt"
  | Shift -> "shift"
  | Ctrl_alt -> "ctrl+alt"
  | Ctrl_shift -> "ctrl+shift"
  | Alt_shift -> "alt+shift"
  | Ctrl_alt_shift -> "ctrl+alt+shift"

type mouse_button =
  | Left
  | Middle
  | Right
  | Wheel_up
  | Wheel_down

type mouse_event_type =
  | Click
  | Release
  | Motion

type mouse_event = {
  button : mouse_button;
  event_type : mouse_event_type;
  x : int;
  y : int;
  ctrl : bool;
  alt : bool;
  shift : bool;
}

type window_size = { width : int; height : int }

type t =
  | KeyDown of key * modifier
  | Mouse of mouse_event
  | Resize of window_size
  | Timer of Timer_ref.t
  | Frame of Time.Instant.t
  | Paste of string
  | Focus_gained
  | Focus_lost
  | Custom of Message.t

let pp fmt = function
  | KeyDown (key, mod_) ->
      let mod_str = modifier_to_string mod_ in
      let key_str = key_to_string key in
      if mod_str = "" then Format.fprintf fmt "KeyDown(%s)" key_str
      else Format.fprintf fmt "KeyDown(%s+%s)" mod_str key_str
  | Mouse { button; event_type; x; y; _ } ->
      let btn =
        match button with
        | Left -> "left"
        | Middle -> "middle"
        | Right -> "right"
        | Wheel_up -> "wheel_up"
        | Wheel_down -> "wheel_down"
      in
      let evt =
        match event_type with
        | Click -> "click"
        | Release -> "release"
        | Motion -> "motion"
      in
      Format.fprintf fmt "Mouse(%s,%s,x=%d,y=%d)" btn evt x y
  | Resize { width; height } ->
      Format.fprintf fmt "Resize(w=%d,h=%d)" width height
  | Timer _ref -> Format.fprintf fmt "Timer(...)"
  | Frame _instant -> Format.fprintf fmt "Frame(...)"
  | Paste content ->
      let preview =
        if String.length content > 20 then
          String.sub content 0 17 ^ "..."
        else content
      in
      Format.fprintf fmt "Paste(%S)" preview
  | Focus_gained -> Format.fprintf fmt "FocusGained"
  | Focus_lost -> Format.fprintf fmt "FocusLost"
  | Custom _msg -> Format.fprintf fmt "Custom"
