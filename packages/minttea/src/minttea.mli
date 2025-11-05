(** Minttea - Elm-style terminal UI framework
    

    Build interactive terminal applications using the Model-View-Update pattern.

*)

open Std

(** Configuration *)
module Config : sig
  type render_mode = Clear | Persist

  type t = {
    render_mode : render_mode;
    fps : int;
    initial_width : int;
    initial_height : int;
  }

  val make :
    ?render_mode:render_mode ->
    ?fps:int ->
    ?initial_width:int ->
    ?initial_height:int ->
    unit ->
    t
end

val config :
  ?render_mode:Config.render_mode ->
  ?fps:int ->
  ?initial_width:int ->
  ?initial_height:int ->
  unit ->
  Config.t
(** Create a configuration with optional parameters *)

(** Terminal events *)
module Event : sig
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

  type mouse_button = Left | Middle | Right | Wheel_up | Wheel_down
  type mouse_event_type = Click | Release | Motion

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
    | Timer of Timer.id Ref.t
    | Frame of Time.Instant.t
    | Paste of string
    | Focus_gained
    | Focus_lost
    | Custom of Message.t

  val key_to_string : key -> string
  val modifier_to_string : modifier -> string
  val pp : Format.formatter -> t -> unit
end

(** Terminal commands *)
module Command : sig
  type mouse_mode = Cell_motion | All_motion

  type t =
    | Noop
    | Quit
    | Hide_cursor
    | Show_cursor
    | Exit_alt_screen
    | Enter_alt_screen
    | Enable_mouse of mouse_mode
    | Disable_mouse
    | Enable_bracketed_paste
    | Disable_bracketed_paste
    | Enable_focus_tracking
    | Disable_focus_tracking
    | Set_window_title of string
    | Batch of t list
    | Sequence of t list
    | Seq of t list
    | Set_timer of { ref : Timer.id Ref.t; duration : Time.Duration.t }
    | Query_window_size

  val batch : t list -> t
  val sequence : t list -> t
  val timer : after:Time.Duration.t -> Timer.id Ref.t * t
  val query_window_size : t
end

(** Declarative layout system *)
module Element : module type of Element

(** Rendering pipeline *)
module Render = Render

(** Styles module for terminal text styling *)
module Style = Style

(** Application definition *)
module App : sig
  type 'model t

  val make :
    init:('model -> 'model * Command.t) ->
    update:(Event.t -> 'model -> 'model * Command.t) ->
    view:('model -> Element.t) ->
    unit ->
    'model t
end

module Component = Component

val app :
  init:('model -> 'model * Command.t) ->
  update:(Event.t -> 'model -> 'model * Command.t) ->
  view:('model -> Element.t) ->
  unit ->
  'model App.t
(** Create a new application *)

val run : ?config:Config.t -> 'model -> 'model App.t -> (unit, Process.exit_reason) result
(** Run the application *)

val start : ?config:Config.t -> 'model App.t -> 'model -> unit
(** Start the application with Miniriot runtime *)
