module Conf = Config

open Std
open Std.IO
open Sync
open Sync.Cell
open Tty

let max_fps = 120

let cap = fun fps -> float_of_int (Int.max 1 (Int.min fps max_fps))

let fps_to_secs = fun fps -> 1. /. cap fps

type mouse_mode =
  | Cell_motion
  | All_motion

type Message.t +=
  | Render of Gooey.Element.t
  | Resize of { width: int; height: int }
  | EnterAltScreen
  | ExitAltScreen
  | Tick
  | Shutdown
  | SetCursorVisibility of [`hidden | `visible]
  | EnableMouse of mouse_mode
  | DisableMouse
  | EnableBracketedPaste
  | DisableBracketedPaste
  | EnableFocusTracking
  | DisableFocusTracking
  | SetWindowTitle of string
  | RendererStarted of Pid.t
  | ShutdownComplete

let schedule_next_tick = fun pid fps ->
  if fps > 0 then
    let after = Time.Duration.from_secs_float (fps_to_secs fps) in
    let _ = Timer.send_after pid Tick ~after in
    ()
  else
    ()

type t = Pid.t

type state = {
  fps: int;
  runner: Pid.t;
  tty: Tty.t;
  render_mode: Conf.render_mode;
  output_target: Conf.output_target;
  mutable lines_rendered: int;
  mutable is_altscreen_active: bool;
  mutable needs_altscreen_setup: bool;
  (* Flag to defer alt screen setup *)
  mutable cursor_visibility: [`hidden | `visible];
  mutable mouse_enabled: bool;
  mutable mouse_mode: mouse_mode option;
  mutable bracketed_paste_enabled: bool;
  mutable focus_tracking_enabled: bool;
  (* Current element to render *)
  mutable current_root_element: Gooey.Element.t;
  (* Frame counter for debugging *)
  mutable frame_count: int;
}

(* Empty element for initialization *)

let empty_element = Gooey.Element.empty

(* Helper functions for output target *)

let write_output = fun state str ->
  match state.output_target with
  | Conf.Stdout -> print str
  | Conf.Stderr -> eprint str

let log_frame = fun _frame_num _frame ->
  (* Frame logging disabled - would require file I/O *)
  ()

let restore_screen = fun t ->
  let output = Buffer.create ~size:64 in
  if t.cursor_visibility = `hidden then
    Buffer.add_string output Tty.Escape_seq.show_cursor_seq;
  if t.mouse_enabled then (
    (* Disable mouse SGR mode *)
    Buffer.add_string output Tty.Escape_seq.disable_mouse_extended_mode_seq;
    (* Disable mouse tracking *)
    match t.mouse_mode with
    | Some Cell_motion -> Buffer.add_string output Tty.Escape_seq.disable_mouse_cell_motion_seq
    | Some All_motion -> Buffer.add_string output Tty.Escape_seq.disable_mouse_all_motion_seq
    | None -> ()
  );
  if t.bracketed_paste_enabled then
    Buffer.add_string output Tty.Escape_seq.disable_bracketed_paste_seq;
  if t.focus_tracking_enabled then
    Buffer.add_string output Tty.Escape_seq.disable_focus_events_seq;
  if Buffer.length output > 0 then
    write_output t (Buffer.contents output)

(* Pipeline: Element -> Gooey Layout -> ANSI (using Gooey) *)

let paint_frame = fun state ->
  let size = Tty.size state.tty in
  Log.debug
    (
      "[PAINT_FRAME] Element type: " ^ (
        match state.current_root_element with
        | Gooey.Element.Empty -> "Empty"
        | Gooey.Element.Text _ -> "Text"
        | Gooey.Element.Container _ -> "Container"
        | Gooey.Element.Custom _ -> "Custom"
      )
    );
  (* Create Gooey viewport from terminal size *)
  let viewport =
    Gooey.Viewport.make ~width:(float_of_int size.cols) ~height:(float_of_int size.rows)
  in
  (* Create Gooey config with text measurer *)
  let text_measurer ~constraints:_ text _style =
    let width = float_of_int (String.length text) in
    let height = 1.0 in
    let size = Gooey.Viewport.make ~width ~height in
    { Gooey.Config.size; lines = [ text ] }
  in
  let config = Gooey.Config.make ~viewport ~text_measurer () in
  (* Run Gooey layout to get render commands *)
  let commands = Gooey.layout ~config state.current_root_element in
  (* Convert render commands to ANSI string - use inline renderer for line-by-line output *)
  Gooey.Terminal_renderer_inline.render_to_string commands

let print_frame = fun state frame ->
  let output = Buffer.create ~size:256 in
  (* Start synchronized update for flicker-free rendering *)
  (* TEMPORARILY DISABLED: Some terminals don't support this *)
  (* Buffer.add_string output "\x1b[?2026h"; *)
  (* Handle alt screen setup if needed *)
  if state.needs_altscreen_setup then (
    Buffer.add_string output Tty.Escape_seq.alt_screen_seq;
    Buffer.add_string output (Tty.Escape_seq.erase_display_seq 2);
    (* 2 = clear entire display *)
    state.needs_altscreen_setup <- false
  );
  if state.is_altscreen_active then (
    (* Alt screen: always position at top-left *)
    Buffer.add_string output (Tty.Escape_seq.cursor_position_seq 1 1)
  ) else (
    (* Inline mode: move cursor up to start of previous render if we had lines *)
    if state.lines_rendered > 1 then
      (
        (* Move up to first line of previous render *)
        Buffer.add_string output (Tty.Escape_seq.cursor_up_seq (state.lines_rendered - 1))
      );
    Buffer.add_string output "\r"
  );
  (* Add the frame content (which includes EraseLineRight from ansi_emitter) *)
  Buffer.add_string output frame;
  (* Count lines in frame for tracking - count newlines directly for performance *)
  let line_count =
    let count = Cell.create 1 in
    String.iter
      (fun c ->
        if c = '\n' then
          count := !count + 1)
      frame;
    !count
  in
  (* If new content is shorter than previous, clear remaining lines below *)
  if line_count < state.lines_rendered then
    Buffer.add_string output (Tty.Escape_seq.erase_display_seq 0);
  state.lines_rendered <- line_count;
  if state.is_altscreen_active then
    (
      (* In alt-screen, position at start of last line *)
      Buffer.add_string output (Tty.Escape_seq.cursor_position_seq line_count 1)
    );
  let final_output = Buffer.contents output in
  (* Log frame for debugging *)
  state.frame_count <- state.frame_count + 1;
  log_frame state.frame_count final_output;
  (* Write everything in one go *)
  write_output state final_output

let rec loop = fun state ->
  Log.trace "[RENDERER] Waiting for message...";
  match receive_any () with
  | Shutdown ->
      Log.trace "[RENDERER] Received Shutdown";
      handle_shutdown state
  | Tick ->
      Log.trace "[RENDERER] Received Tick";
      handle_tick state
  | Render element ->
      Log.trace "[RENDERER] Received Render";
      handle_render_element state element;
      loop state
  | SetCursorVisibility cursor ->
      handle_set_cursor_visibility cursor state;
      loop state
  | EnterAltScreen ->
      handle_enter_alt_screen state;
      loop state
  | ExitAltScreen ->
      handle_exit_alt_screen state;
      loop state
  | EnableMouse mode ->
      handle_enable_mouse state mode;
      loop state
  | DisableMouse ->
      handle_disable_mouse state;
      loop state
  | EnableBracketedPaste ->
      handle_enable_bracketed_paste state;
      loop state
  | DisableBracketedPaste ->
      handle_disable_bracketed_paste state;
      loop state
  | EnableFocusTracking ->
      handle_enable_focus_tracking state;
      loop state
  | DisableFocusTracking ->
      handle_disable_focus_tracking state;
      loop state
  | SetWindowTitle title ->
      handle_set_window_title state title;
      loop state
  | _ -> loop state

and handle_shutdown = fun state ->
  (* Clear the last rendered content in inline mode *)
  (
    try
      if
        state.render_mode = Conf.Clear && (not state.is_altscreen_active) && state.lines_rendered
        > 0
      then (
        (* Use the same clearing logic as flush() *)
        (* Move cursor up to the first line *)
        for _i = 1 to state.lines_rendered - 1 do
          write_output state (Tty.Escape_seq.cursor_up_seq 1)
        done;
        (* Clear each line and move down *)
        for i = 1 to state.lines_rendered do
          write_output state Tty.Escape_seq.erase_entire_line_seq;
          if i < state.lines_rendered then
            write_output state (Tty.Escape_seq.cursor_down_seq 1)
        done;
        (* Move cursor back up to where we started (before any content was rendered) *)
        for _i = 1 to state.lines_rendered - 1 do
          write_output state (Tty.Escape_seq.cursor_up_seq 1)
        done;
        write_output state "\r"
      )
    with
    | Sys_blocked_io -> ()
  );
  restore_screen state;
  send state.runner ShutdownComplete;
  Ok ()

and handle_tick = fun t ->
  Log.trace "[RENDERER] Tick received";
  let now = Time.Instant.now () in
  Log.trace "[RENDERER] Sending Frame event to program";
  send t.runner (Io_loop.Input (Event.Frame now));
  schedule_next_tick (self ()) t.fps;
  loop t

and handle_render_element = fun t element ->
  (* Paint immediately so inline apps only redraw when their model changes. *)
  t.current_root_element <- element;
  let frame = paint_frame t in
  print_frame t frame

and handle_enter_alt_screen = fun t ->
  if t.is_altscreen_active then
    ()
  else (
    Log.debug "[RENDERER] Entering alt screen mode";
    t.is_altscreen_active <- true;
    t.needs_altscreen_setup <- true
  )

and handle_exit_alt_screen = fun t ->
  if not t.is_altscreen_active then
    ()
  else (
    t.is_altscreen_active <- false;
    write_output t Tty.Escape_seq.exit_alt_screen_seq
  )

and handle_set_cursor_visibility = fun cursor t ->
  if t.cursor_visibility = cursor then
    ()
  else (
    (
      match cursor with
      | `hidden -> write_output t Tty.Escape_seq.hide_cursor_seq
      | `visible -> write_output t Tty.Escape_seq.show_cursor_seq
    );
    t.cursor_visibility <- cursor
  )

and handle_enable_mouse = fun t mode ->
  if not t.mouse_enabled then (
    (
      match mode with
      | Cell_motion -> write_output t Tty.Escape_seq.enable_mouse_cell_motion_seq
      | All_motion -> write_output t Tty.Escape_seq.enable_mouse_all_motion_seq
    );
    write_output t Tty.Escape_seq.enable_mouse_extended_mode_seq;
    (* SGR mode *)
    t.mouse_enabled <- true;
    t.mouse_mode <- Some mode
  )

and handle_disable_mouse = fun t ->
  if t.mouse_enabled then (
    write_output t Tty.Escape_seq.disable_mouse_all_motion_seq;
    write_output t Tty.Escape_seq.disable_mouse_cell_motion_seq;
    write_output t Tty.Escape_seq.disable_mouse_extended_mode_seq;
    t.mouse_enabled <- false;
    t.mouse_mode <- None
  )

and handle_enable_bracketed_paste = fun t ->
  if not t.bracketed_paste_enabled then (
    write_output t Tty.Escape_seq.enable_bracketed_paste_seq;
    t.bracketed_paste_enabled <- true
  )

and handle_disable_bracketed_paste = fun t ->
  if t.bracketed_paste_enabled then (
    write_output t Tty.Escape_seq.disable_bracketed_paste_seq;
    t.bracketed_paste_enabled <- false
  )

and handle_enable_focus_tracking = fun t ->
  if not t.focus_tracking_enabled then (
    write_output t Tty.Escape_seq.enable_focus_events_seq;
    t.focus_tracking_enabled <- true
  )

and handle_disable_focus_tracking = fun t ->
  if t.focus_tracking_enabled then (
    write_output t Tty.Escape_seq.disable_focus_events_seq;
    t.focus_tracking_enabled <- false
  )

and handle_set_window_title = fun state title ->
  (* OSC 2 ; title BEL *)
  write_output state ("\x1b]2;" ^ title ^ "\x07")

let init = fun ~parent ~config ~tty ->
  send parent (RendererStarted (self ()));
  let Conf.{ render_mode; fps; output } = config in
  schedule_next_tick (self ()) fps;
  let _size = Tty.size tty in
  loop
    {
      fps;
      runner = parent;
      tty;
      is_altscreen_active = false;
      needs_altscreen_setup = false;
      lines_rendered = 0;
      cursor_visibility = `visible;
      render_mode;
      output_target = output;
      mouse_enabled = false;
      mouse_mode = None;
      bracketed_paste_enabled = false;
      focus_tracking_enabled = false;
      current_root_element = empty_element;
      frame_count = 0;
    }

let start = fun ~config ~tty () ->
  let parent = self () in
  let pid = spawn (fun () -> init ~parent ~config ~tty) in
  let selector msg =
    match msg with
    | RendererStarted pid' when Pid.equal pid pid' -> `select pid
    | _ -> `skip
  in
  let timeout = Time.Duration.from_secs 2 in
  receive ~selector ~timeout ()

let render = fun pid element -> send pid (Render element)

let resize = fun pid ~width ~height -> send pid (Resize { width; height })

let enter_alt_screen = fun pid -> send pid EnterAltScreen

let exit_alt_screen = fun pid -> send pid ExitAltScreen

let shutdown = fun pid ->
  send pid Shutdown;
  let selector msg =
    match msg with
    | ShutdownComplete -> `select ()
    | _ -> `skip
  in
  let timeout = Time.Duration.from_secs 2 in
  receive ~selector ~timeout ()

let hide_cursor = fun pid -> send pid (SetCursorVisibility `hidden)

let show_cursor = fun pid -> send pid (SetCursorVisibility `visible)

let enable_mouse = fun pid mode -> send pid (EnableMouse mode)

let disable_mouse = fun pid -> send pid DisableMouse

let enable_bracketed_paste = fun pid -> send pid EnableBracketedPaste

let disable_bracketed_paste = fun pid -> send pid DisableBracketedPaste

let enable_focus_tracking = fun pid -> send pid EnableFocusTracking

let disable_focus_tracking = fun pid -> send pid DisableFocusTracking

let set_window_title = fun pid title -> send pid (SetWindowTitle title)
