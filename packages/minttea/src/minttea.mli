(** Elm-style terminal UI framework for interactive terminal applications. *)
open Std

module Config: sig
  type render_mode =
    | Clear
    | Persist
  type output_target =
    | Stdout
    | Stderr
  type t = {
    render_mode: render_mode;
    fps: int;
    output: output_target;
  }

  val make: ?render_mode:render_mode -> ?fps:int -> ?output:output_target -> unit -> t
end

(** Create a configuration with optional parameters *)
val config:
  ?render_mode:Config.render_mode ->
  ?fps:int ->
  ?output:Config.output_target ->
  unit ->
  Config.t

(** Terminal events *)
module Event: sig
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

  val key_to_string: key -> string

  val modifier_to_string: modifier -> string
end

(** Terminal commands *)
module Command: sig
  type mouse_mode =
    | Cell_motion
    | All_motion
  type t =
    | Noop
    | Quit
    | HideCursor
    | ShowCursor
    | ExitAltScreen
    | EnterAltScreen
    | EnableMouse of mouse_mode
    | DisableMouse
    | EnableBracketedPaste
    | DisableBracketedPaste
    | EnableFocusTracking
    | DisableFocusTracking
    | SetWindowTitle of string
    | Seq of t list
    | SetTimer of {
        ref: Timer.id Ref.t;
        duration: Time.Duration.t;
      }

  val timer: after:Time.Duration.t -> Timer.id Ref.t * t
end

(** Declarative layout system - re-exported from Gooey *)
module Element = Gooey.Element

(** Styles module for terminal text styling - re-exported from Gooey *)
module Style = Gooey.Style

(** Application definition *)
module App: sig
  type 'model t

  val make:
    init:('model -> 'model * Command.t) ->
    update:(Event.t -> 'model -> 'model * Command.t) ->
    view:('model -> Gooey.Element.t) ->
    unit ->
    'model t
end

module Component = Component

(** Create a new application *)
val app:
  init:('model -> 'model * Command.t) ->
  update:(Event.t -> 'model -> 'model * Command.t) ->
  view:('model -> Gooey.Element.t) ->
  unit ->
  'model App.t

(** Run the application *)
val run: ?config:Config.t -> 'model -> 'model App.t -> (unit, exn) result

(** Start the application with Std.Runtime *)
val start: ?config:Config.t -> 'model App.t -> 'model -> unit
