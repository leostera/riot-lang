open Std

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
  | Set_timer of Timer_ref.t * float
  | Query_window_size

let batch cmds = Batch cmds
let sequence cmds = Sequence cmds

let timer ~after =
  let ref = Timer_ref.make () in
  (ref, Set_timer (ref, after))

let query_window_size = Query_window_size
