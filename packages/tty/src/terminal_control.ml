open Std

(* Synchronized updates *)
let begin_synchronized_update () =
  print_string "\x1b[?2026h";
  flush stdout

let end_synchronized_update () =
  print_string "\x1b[?2026l";
  flush stdout

(* Cursor styles *)
type cursor_style =
  | DefaultUserShape
  | BlinkingBlock
  | SteadyBlock
  | BlinkingUnderScore
  | SteadyUnderScore
  | BlinkingBar
  | SteadyBar

let set_cursor_style style =
  let seq =
    match style with
    | DefaultUserShape -> "\x1b[0 q"
    | BlinkingBlock -> "\x1b[1 q"
    | SteadyBlock -> "\x1b[2 q"
    | BlinkingUnderScore -> "\x1b[3 q"
    | SteadyUnderScore -> "\x1b[4 q"
    | BlinkingBar -> "\x1b[5 q"
    | SteadyBar -> "\x1b[6 q"
  in
  print_string seq;
  flush stdout

(* Line wrapping *)
let enable_line_wrap () =
  print_string "\x1b[?7h";
  flush stdout

let disable_line_wrap () =
  print_string "\x1b[?7l";
  flush stdout

(* Window size *)
type window_size = {
  rows : int;
  columns : int;
  width_px : int;
  height_px : int;
}

let window_size () =
  match Size.get () with
  | Ok size ->
      { rows = size.rows; columns = size.cols; width_px = 0; height_px = 0 }
  | Error _ ->
      (* Fallback to 80x24 if size query fails *)
      { rows = 24; columns = 80; width_px = 0; height_px = 0 }

(* Raw mode check - track state via global ref *)
let raw_mode_enabled = ref false

let is_raw_mode_enabled () = !raw_mode_enabled

(* Hook into Terminal module's raw mode functions *)
(* This would require modifying terminal.ml to call our tracking functions *)
