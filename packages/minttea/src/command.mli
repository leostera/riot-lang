(** Commands for controlling terminal application behavior *)

open Std

(** Mouse tracking mode *)
type mouse_mode =
  | Cell_motion  (** Track mouse with button pressed (drag events) *)
  | All_motion   (** Track all mouse movement (hover events) *)

(** Terminal commands *)
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
  | Batch of t list  (** Execute commands concurrently *)
  | Sequence of t list  (** Execute commands sequentially *)
  | Seq of t list  (** @deprecated Use Batch or Sequence *)
  | Set_timer of Timer_ref.t * float
  | Query_window_size  (** Query terminal window size - responds with WindowSize message *)

(** Helper functions *)

val batch : t list -> t
(** Create a batch command that executes concurrently *)

val sequence : t list -> t
(** Create a sequence command that executes in order *)

val timer : after:float -> Timer_ref.t * t
(** Create a timer command with a new timer reference *)

val query_window_size : t
(** Create a command to query the current terminal window size.
    
    When executed, this will generate a WindowSize message with
    the current terminal dimensions. *)
