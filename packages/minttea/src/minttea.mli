(** Minttea - Elm-style terminal UI framework
    
    Build interactive terminal applications using the Model-View-Update pattern.

*)

open Std

(** Timer references for unique timer identification *)
module Timer : sig
  type t
  val equal : t -> t -> bool
end

(** Configuration *)
module Config : sig
  type t = {
    render_mode : [ `clear | `persist ];
    fps : int;
  }

  val make :
    ?render_mode:[ `clear | `persist ] ->
    ?fps:int ->
    unit ->
    t
end

val config :
  ?render_mode:[ `clear | `persist ] ->
  ?fps:int ->
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
    | Timer of Timer.t
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
    | Set_timer of Timer.t * float
    | Query_window_size

  val batch : t list -> t
  val sequence : t list -> t
  val timer : after:float -> Timer.t * t
  val query_window_size : t
end

(** Styles module for terminal text styling *)
module Style : sig
(** Layout composition utilities *)
module Layout : sig
  val join_horizontal : pos:[`Top | `Center | `Bottom] -> string list -> string
  (** Place strings side-by-side horizontally *)

  val join_vertical : pos:[`Left | `Center | `Right] -> string list -> string
  (** Stack strings vertically *)

  val place : 
    width:int -> 
    height:int -> 
    h_pos:float -> 
    v_pos:float -> 
    string -> 
    string
  (** Position string within a box using fractional coordinates *)
end

  type color = Tty.Color.t = private
    | RGB of int * int * int
    | ANSI of int
    | ANSI256 of int
    | No_color

  val color : ?profile:Tty.Profile.t -> string -> color
  val gradient : start:color -> finish:color -> steps:int -> color array

  module Border : sig
    type t

    val make :
      ?top:string ->
      ?left:string ->
      ?bottom:string ->
      ?right:string ->
      ?top_left:string ->
      ?top_right:string ->
      ?bottom_left:string ->
      ?bottom_right:string ->
      ?middle_left:string ->
      ?middle_right:string ->
      ?middle:string ->
      ?middle_top:string ->
      ?middle_bottom:string ->
      unit ->
      t

    val normal : t
    val rounded : t
    val block : t
    val outer_half_block : t
    val inner_half_block : t
    val thick : t
    val double : t
    val hidden : t
  end

  type style

  val default : style
  val bg : color -> style -> style
  val blink : bool -> style -> style
  val bold : bool -> style -> style
  val faint : bool -> style -> style
  val fg : color -> style -> style
  val height : int -> style -> style
  val italic : bool -> style -> style
  val margin_bottom : int -> style -> style
  val margin_left : int -> style -> style
  val margin_right : int -> style -> style
  val margin_top : int -> style -> style
  val max_height : int -> style -> style
  val max_width : int -> style -> style
  val padding_bottom : int -> style -> style
  val padding_left : int -> style -> style
  val padding_right : int -> style -> style
  val padding_top : int -> style -> style
  val reverse : bool -> style -> style
  val strikethrough : bool -> style -> style
  val underline : bool -> style -> style
  val width : int option -> style -> style
  val border : Border.t -> style -> style
  val align_horizontal : [`Left | `Center | `Right] -> style -> style
  val align_vertical : [`Top | `Center | `Bottom] -> style -> style
  val render : style -> string -> string
end

(** Application definition *)
module App : sig
  type 'model t

  val make :
    init:('model -> Command.t) ->
    update:(Event.t -> 'model -> 'model * Command.t) ->
    view:('model -> string) ->
    unit ->
    'model t
end

module Component = Component

val app :
  init:('model -> Command.t) ->
  update:(Event.t -> 'model -> 'model * Command.t) ->
  view:('model -> string) ->
  unit ->
  'model App.t
(** Create a new application *)

val run : ?config:Config.t -> 'model -> 'model App.t -> unit
(** Run the application *)

val start : ?config:Config.t -> 'model App.t -> 'model -> unit
(** Start the application with Miniriot runtime *)
