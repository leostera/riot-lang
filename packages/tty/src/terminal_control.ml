open Std

(* Synchronized updates *)

let begin_synchronized_update = fun t -> Terminal.write_escape t "?2026h"

let end_synchronized_update = fun t -> Terminal.write_escape t "?2026l"

(* Cursor styles *)

type cursor_style =
  | DefaultUserShape
  | BlinkingBlock
  | SteadyBlock
  | BlinkingUnderScore
  | SteadyUnderScore
  | BlinkingBar
  | SteadyBar

let set_cursor_style = fun t style ->
  let code =
    match style with
    | DefaultUserShape -> "0 q"
    | BlinkingBlock -> "1 q"
    | SteadyBlock -> "2 q"
    | BlinkingUnderScore -> "3 q"
    | SteadyUnderScore -> "4 q"
    | BlinkingBar -> "5 q"
    | SteadyBar -> "6 q"
  in
  Terminal.write_escape t code

(* Line wrapping *)

let enable_line_wrap = fun t -> Terminal.write_escape t "?7h"

let disable_line_wrap = fun t -> Terminal.write_escape t "?7l"

(* Window size *)

type window_size = { rows: int; columns: int; width_px: int; height_px: int }

let window_size = fun t ->
  let size = Terminal.(t.size) in
  {
    rows = size.rows;
    columns = size.cols;
    width_px = 0;
    height_px = 0;
  }

(* Raw mode check *)

let is_raw_mode_enabled = fun t ->
  match Terminal.(t.mode) with
  | Terminal.Immediate -> true
  | Terminal.LineBuffered -> false
