open Std

(** Terminal renderer for Minttea *)

type t = Pid.t
(** Handle to a renderer process *)

type mouse_mode = Cell_motion | All_motion

type Message.t +=
  | Render of string
  | Enter_alt_screen
  | Exit_alt_screen
  | Tick
  | Shutdown
  | Set_cursor_visibility of [ `hidden | `visible ]
  | Enable_mouse of mouse_mode
  | Disable_mouse
  | Enable_bracketed_paste
  | Disable_bracketed_paste
  | Enable_focus_tracking
  | Disable_focus_tracking
  | Set_window_title of string
  | RendererStarted of Pid.t
  | ShutdownComplete
(** Renderer message types *)

val start : config:Config.t -> unit -> t
(** Start a renderer process. Sends RendererStarted message to parent. *)

val render : t -> string -> unit
(** Send output to be rendered *)

val enter_alt_screen : t -> unit
(** Enter alternate screen mode *)

val exit_alt_screen : t -> unit
(** Exit alternate screen mode *)

val shutdown : t -> unit
(** Shutdown the renderer *)

val hide_cursor : t -> unit
(** Hide the cursor *)

val show_cursor : t -> unit
(** Show the cursor *)

val enable_mouse : t -> mouse_mode -> unit
(** Enable mouse tracking *)

val disable_mouse : t -> unit
(** Disable mouse tracking *)

val enable_bracketed_paste : t -> unit
(** Enable bracketed paste mode *)

val disable_bracketed_paste : t -> unit
(** Disable bracketed paste mode *)

val enable_focus_tracking : t -> unit
(** Enable focus reporting *)

val disable_focus_tracking : t -> unit
(** Disable focus reporting *)

val set_window_title : t -> string -> unit
(** Set the terminal window title *)
