(** Terminal input event parsing.

    This module parses raw terminal input into structured events including
    keyboard presses with modifiers, mouse events, focus changes, and more.
    It handles ANSI/VT escape sequence parsing for modern terminal features.

    {1 Example: Basic Event Loop}

    {[
      open Std
      open Tty

      let rec event_loop () =
        match Input.read_event () with
        | `Key (Input.Char 'q', []) -> 
            println "Quit!";
        | `Key (Input.Char 's', [Ctrl]) -> 
            println "Save (Ctrl+S)";
        | `Mouse { button = Left; action = Press; x; y; _ } ->
            println (format "Click at %d,%d" x y);
            event_loop ()
        | `Resize (width, height) ->
            println (format "Terminal resized to %dx%d" width height);
            event_loop ()
        | `Unknown seq ->
            println (format "Unknown sequence: %S" seq);
            event_loop ()
        | `Retry -> event_loop ()
        | `End -> ()

      let () =
        let old = Stdin.setup () in
        Escape_seq.enable_mouse_all_motion_seq ();
        Escape_seq.enable_bracketed_paste_seq ();
        event_loop ();
        Stdin.shutdown old
    ]}

    {1 Example: Handling Paste Events}

    {[
      match Input.read_event () with
      | `Paste content ->
          (* Content from bracketed paste *)
          insert_text content
      | _ -> ()
    ]} *)
(** {1 Types} *)

type key =
  | Char of char
  (** Regular character *)
  | Enter
  | Tab
  | BackTab
  (** Shift+Tab *)
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
  (** Function keys F1-F12 *)
  | CapsLock
  | ScrollLock
  | NumLock
  | PrintScreen
  | Pause
  | Menu
  | KeypadBegin
  (** Often keypad 5 with NumLock off *)
  | Media of media_key

(** Media control keys *)
(** Media control keys *)
and media_key =
  | Play
  | Pause_media
  (** Named to avoid conflict with Pause key *)
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
type modifier =
  | Shift
  | Alt
  | Ctrl
  | Meta
  | Super
  (** Windows/Command key *)
  | Hyper
type mouse_button =
  | Left
  | Middle
  | Right
  | ScrollUp
  | ScrollDown
  | ScrollLeft
  (** Touchpad horizontal scroll *)
  | ScrollRight
(** Touchpad horizontal scroll *)
type mouse_action =
  | Mouse_press
  (** Mouse button pressed *)
  | Mouse_release
  (** Mouse button released *)
  | Mouse_drag
  (** Mouse moved with button held *)
  | Mouse_move
(** Mouse moved without button pressed *)
type mouse_event = {
  button: mouse_button;
  action: mouse_action;
  x: int;  (** Column (1-based) *)
  y: int;  (** Row (1-based) *)
  modifiers: modifier list;
}
(** Key event kind distinguishes press, release, and repeat *)
type key_event_kind =
  | Press
  (** Key pressed *)
  | Release
  (** Key released *)
  | Repeat
(** Key auto-repeat *)
(** Keyboard event with kind information *)
type key_event = {
  code: key;
  modifiers: modifier list;
  kind: key_event_kind;
}
(** Terminal events *)
type event =
[
  `Key of key_event
  | `Mouse of mouse_event
  | `Resize of int * int
  (** width × height *)
  | `Paste of string
  (** Bracketed paste content *)
  | `FocusGained
  | `FocusLost
  | `Unknown of string
  (** Unknown escape sequence *)
  | `Retry
  (** No data available, try again *)
  | `End
]
(** {1 Reading Events} *)
val read_event: unit -> event

(** [read_event ()] reads and parses the next terminal event.

    This is a non-blocking read that returns immediately with [`Retry] if no
    input is available. It handles:
    - Simple keypresses and characters
    - Modified keys (Ctrl, Alt, Shift combinations)
    - ANSI escape sequences (arrow keys, function keys, etc.)
    - Mouse events (if mouse tracking is enabled)
    - Window resize events (SIGWINCH)
    - Bracketed paste (if enabled)
    - Focus events (if enabled)

    The terminal must be in raw mode (call {!Stdin.setup} first).
    Enable mouse tracking, bracketed paste, or focus tracking separately
    using functions from {!Escape_seq} module. *)
val try_read: unit -> event option

(** [try_read ()] attempts to read an event without blocking.
    
    Returns [None] if no event is available, [Some event] otherwise.
    This is a convenience wrapper around {!read_event} that filters
    out [`Retry] and [`End] results. *)
val parse_escape: string -> event option

(** [parse_escape seq] parses an ANSI escape sequence into an event.

    Returns [None] if the sequence is incomplete or unrecognized.
    This is used internally by {!read_event} but exposed for testing. *)
val key_to_string: key -> string

(** [key_to_string key] returns a human-readable name for the key. *)
val modifier_to_string: modifier -> string

(** [modifier_to_string mod] returns a human-readable name for the modifier. *)
val button_to_string: mouse_button -> string

(** [button_to_string btn] returns a human-readable name for the mouse button. *)
(** {1 Event Formatting} *)

val event_to_string: event -> string

(** [event_to_string event] converts an event to a readable string. *)
