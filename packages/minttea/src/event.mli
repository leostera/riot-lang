(** Terminal events for Minttea applications *)
open Std

(** Keyboard modifiers *)
type modifier =
  | NoModifier
  | Ctrl
  | Alt
  | Shift
  | CtrlAlt
  | CtrlShift
  | AltShift
  | CtrlAltShift
(** Keyboard keys *)
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
  (** Function keys F1-F12 *)
  | Key of string
(** Any other character(s) *)
val key_to_string : key -> string

(** Convert a key to a human-readable string *)
val modifier_to_string : modifier -> string

(** Convert a modifier to a human-readable string *)
(** Mouse button *)
type mouse_button =
  | Left
  | Middle
  | Right
  | WheelUp
  | WheelDown
(** Mouse event type *)
type mouse_event_type =
  | Click
  (** Mouse button pressed *)
  | Release
  (** Mouse button released *)
  | Motion
(** Mouse moved (with or without button pressed) *)
(** Mouse event *)
type mouse_event = {
  button : mouse_button;
  event_type : mouse_event_type;
  x : int;  (** Column position (0-based) *)
  y : int;  (** Row position (0-based) *)
  ctrl : bool;
  alt : bool;
  shift : bool;
}
(** Window size *)
type window_size = {
  width : int;  (** Terminal width in columns *)
  height : int;  (** Terminal height in rows *)
}
(** Terminal events *)
type t =
  | KeyDown of key * modifier
  | Mouse of mouse_event
  | Resize of window_size
  | Timer of Timer.id Ref.t
  | Frame of Time.Instant.t
  | Paste of string
  (** Bracketed paste content *)
  | FocusGained
  | FocusLost
  | Custom of Message.t
val to_string : t -> string

(** Convert an event to a human-readable string *)
