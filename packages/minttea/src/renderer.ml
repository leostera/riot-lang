open Std
open Tty

let max_fps = 120
let cap fps = Int.max 1 (Int.min fps max_fps) |> Int.to_float
let fps_to_secs fps = 1. /. cap fps

type mouse_mode = Cell_motion | All_motion

type Message.t +=
  | Render of Element.t
  | Resize of { width : int; height : int }
  | EnterAltScreen
  | ExitAltScreen
  | Tick
  | Shutdown
  | SetCursorVisibility of [ `hidden | `visible ]
  | EnableMouse of mouse_mode
  | DisableMouse
  | EnableBracketedPaste
  | DisableBracketedPaste
  | EnableFocusTracking
  | DisableFocusTracking
  | SetWindowTitle of string
  | RendererStarted of Pid.t
  | ShutdownComplete

type t = Pid.t

type state = {
  fps : int;
  runner : Pid.t;
  tty : Tty.t;
  render_mode : Config.render_mode;
  output_target : Config.output_target;
  mutable lines_rendered : int;
  mutable is_altscreen_active : bool;
  mutable needs_altscreen_setup : bool; (* Flag to defer alt screen setup *)
  mutable cursor_visibility : [ `hidden | `visible ];
  mutable mouse_enabled : bool;
  mutable mouse_mode : mouse_mode option;
  mutable bracketed_paste_enabled : bool;
  mutable focus_tracking_enabled : bool;
  (* Current element to render *)
  mutable current_root_element : Element.t;
  matrix : Render.Matrix.t;
  (* Frame counter for debugging *)
  mutable frame_count : int;
}

(* Empty element for initialization *)
let empty_element = Element.Empty

(* Helper functions for output target *)
let frame_log_channel = ref None

let write_output state str =
  match state.output_target with
  | Config.Stdout -> print "%s%!" str  (* %! forces flush *)
  | Config.Stderr -> eprint "%s%!" str

let log_frame frame_num frame =
  try
    let ch = match !frame_log_channel with
      | Some ch -> ch
      | None ->
          let ch = open_out_gen [Open_creat; Open_wronly; Open_append; Open_text] 0o644 "frames.log" in
          frame_log_channel := Some ch;
          ch
    in
    Printf.fprintf ch "========== FRAME %d ==========\n" frame_num;
    (* Write the raw frame with escape sequences visible *)
    String.iter (fun c ->
      match c with
      | '\x1b' -> output_string ch "\\x1b"
      | '\n' -> output_string ch "\\n\n"
      | '\r' -> output_string ch "\\r"
      | c -> output_char ch c
    ) frame;
    Printf.fprintf ch "\n========== END FRAME %d ==========\n\n" frame_num;
    flush ch
  with _ -> ()

let restore_screen t =
  let output = Buffer.create 64 in

  if t.cursor_visibility = `hidden then
    Buffer.add_string output Tty.Escape_seq.show_cursor_seq;

  if t.mouse_enabled then (
    (* Disable mouse SGR mode *)
    Buffer.add_string output Tty.Escape_seq.disable_mouse_extended_mode_seq;
    (* Disable mouse tracking *)
    match t.mouse_mode with
    | Some Cell_motion ->
        Buffer.add_string output Tty.Escape_seq.disable_mouse_cell_motion_seq
    | Some All_motion ->
        Buffer.add_string output Tty.Escape_seq.disable_mouse_all_motion_seq
    | None -> ());

  if t.bracketed_paste_enabled then
    Buffer.add_string output Tty.Escape_seq.disable_bracketed_paste_seq;

  if t.focus_tracking_enabled then
    Buffer.add_string output Tty.Escape_seq.disable_focus_events_seq;

  (* Write to configured output *)
  if Buffer.length output > 0 then write_output t (Buffer.contents output)

(* Pipeline: Element -> Scene -> Matrix -> ANSI (expensive, only when needed) *)
let paint_frame state =
  let size = Tty.size state.tty in
  
  Log.debug "[PAINT_FRAME] Element type: %s" 
    (match state.current_root_element with
    | Element.Empty -> "Empty"
    | Element.Text _ -> "Text"
    | Element.Box _ -> "Box"
    | Element.Spacer _ -> "Spacer"
    | Element.Row _ -> "Row"
    | Element.Column _ -> "Column"
    | Element.Layer _ -> "Layer");

  (* Convert Element to Scene graph *)
  let scene =
    (* Create layout context *)
    let ctx =
      Render.Layout.
        {
          x = 0;
          y = 0;
          available_width = size.cols;
          available_height = size.rows;
        }
    in
    Render.Layout.to_scene state.current_root_element ctx
  in

  (* Paint the scene onto the matrix buffer *)
  let matrix =
    (* Flatten the scene graph and sort by z-index *)
    let flattened = Render.Scene.flatten scene in
    Render.Painter.paint ~matrix:state.matrix ~scene:flattened;
    state.matrix
  in

  (* Convert matrix to ANSI escape sequences *)
  let frame =
    (* Determine render mode based on screen state *)
    let mode =
      if state.is_altscreen_active then Render.Ansi_emitter.Fullscreen
      else Render.Ansi_emitter.ContentFit
    in
    Render.Ansi_emitter.emit matrix ~mode
  in

  frame

let print_frame state frame =
  let output = Buffer.create 256 in
  
  (* Start synchronized update for flicker-free rendering *)
  (* TEMPORARILY DISABLED: Some terminals don't support this *)
  (* Buffer.add_string output "\x1b[?2026h"; *)
  
  (* Handle alt screen setup if needed *)
  if state.needs_altscreen_setup then (
    Buffer.add_string output Tty.Escape_seq.alt_screen_seq;
    Buffer.add_string output (Tty.Escape_seq.erase_display_seq 2); (* 2 = clear entire display *)
    state.needs_altscreen_setup <- false
  );
  
  (* Position cursor based on mode *)
  if state.is_altscreen_active then (
    (* Alt screen: always position at top-left *)
    Buffer.add_string output (Tty.Escape_seq.cursor_position_seq 1 1)
  ) else (
    (* Inline mode: move cursor up to start of previous render if we had lines *)
    if state.lines_rendered > 1 then (
      (* Move up to first line of previous render *)
      Buffer.add_string output (Tty.Escape_seq.cursor_up_seq (state.lines_rendered - 1))
    );
    Buffer.add_string output "\r"
  );
  
  (* Add the frame content (which includes EraseLineRight from ansi_emitter) *)
  Buffer.add_string output frame;
  
  (* Count lines in frame for tracking - count newlines directly for performance *)
  let line_count = 
    let count = ref 1 in
    String.iter (fun c -> if c = '\n' then incr count) frame;
    !count
  in
  
  (* If new content is shorter than previous, clear remaining lines below *)
  if line_count < state.lines_rendered then (
    Buffer.add_string output (Tty.Escape_seq.erase_display_seq 0) (* 0 = erase from cursor to end of screen *)
  );
  
  (* Update lines_rendered count *)
  state.lines_rendered <- line_count;
  
  (* Position cursor at start of last line for consistency (Bubbletea does this) *)
  if state.is_altscreen_active then (
    (* In alt-screen, position at start of last line *)
    Buffer.add_string output (Tty.Escape_seq.cursor_position_seq line_count 1)
  ) else (
    (* In inline mode, just carriage return *)
    Buffer.add_string output "\r"
  );
  
  (* End synchronized update *)
  (* TEMPORARILY DISABLED: Some terminals don't support this *)
  (* Buffer.add_string output "\x1b[?2026l"; *)
  
  let final_output = Buffer.contents output in
  
  (* Log frame for debugging *)
  state.frame_count <- state.frame_count + 1;
  log_frame state.frame_count final_output;
  
  (* Write everything in one go *)
  write_output state final_output

let rec loop state =
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

and handle_shutdown state =
  (* Make output blocking temporarily for shutdown to avoid Sys_blocked_io *)
  (try
     let fd =
       match state.output_target with
       | Config.Stdout -> Kernel.Fd.make_blocking Unix.stdout
       | Config.Stderr -> Kernel.Fd.make_blocking Unix.stderr
     in
     Kernel.Fd.set_blocking fd
   with _ -> ());

  (* Clear the last rendered content in inline mode *)
  (try
     if
       state.render_mode = Config.Clear
       && (not state.is_altscreen_active)
       && state.lines_rendered > 0
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
       write_output state "\r" (* move to column 0 *))
   with Sys_blocked_io -> ());
  restore_screen state;
  send state.runner ShutdownComplete;
  Ok ()

and handle_tick t =
  Log.trace "[RENDERER] Tick received";
  let now = Time.Instant.now () in

  (* Always render on every tick to ensure EraseLineRight sequences are emitted.
     This follows Bubbletea's approach of rendering every frame at the configured FPS.
     The differential optimization at the element level wasn't working correctly. *)
  Log.trace "[RENDERER] Painting frame";
  let frame = paint_frame t in
  print_frame t frame;

  (* Always send frame event for app updates *)
  Log.trace "[RENDERER] Sending Frame event to program";
  send t.runner (Io_loop.Input (Event.Frame now));

  let _ =
    let after = Time.Duration.from_secs_float (fps_to_secs t.fps) in
    Timer.send_after (self ()) Tick ~after
  in

  loop t

and handle_render_element t element =
  (* Update current element - will be painted on next tick *)
  t.current_root_element <- element

and handle_enter_alt_screen t =
  if t.is_altscreen_active then ()
  else (
    Log.debug "[RENDERER] Entering alt screen mode";
    t.is_altscreen_active <- true;
    t.needs_altscreen_setup <- true (* Set flag to add setup on flush *))

and handle_exit_alt_screen t =
  if not t.is_altscreen_active then ()
  else (
    t.is_altscreen_active <- false;
    write_output t Tty.Escape_seq.exit_alt_screen_seq)

and handle_set_cursor_visibility cursor t =
  if t.cursor_visibility = cursor then ()
  else (
    (match cursor with
    | `hidden -> write_output t Tty.Escape_seq.hide_cursor_seq
    | `visible -> write_output t Tty.Escape_seq.show_cursor_seq);
    t.cursor_visibility <- cursor)

and handle_enable_mouse t mode =
  if not t.mouse_enabled then (
    (match mode with
    | Cell_motion -> write_output t Tty.Escape_seq.enable_mouse_cell_motion_seq
    | All_motion -> write_output t Tty.Escape_seq.enable_mouse_all_motion_seq);
    write_output t Tty.Escape_seq.enable_mouse_extended_mode_seq;
    (* SGR mode *)
    t.mouse_enabled <- true;
    t.mouse_mode <- Some mode)

and handle_disable_mouse t =
  if t.mouse_enabled then (
    write_output t Tty.Escape_seq.disable_mouse_all_motion_seq;
    write_output t Tty.Escape_seq.disable_mouse_cell_motion_seq;
    write_output t Tty.Escape_seq.disable_mouse_extended_mode_seq;
    t.mouse_enabled <- false;
    t.mouse_mode <- None)

and handle_enable_bracketed_paste t =
  if not t.bracketed_paste_enabled then (
    write_output t Tty.Escape_seq.enable_bracketed_paste_seq;
    t.bracketed_paste_enabled <- true)

and handle_disable_bracketed_paste t =
  if t.bracketed_paste_enabled then (
    write_output t Tty.Escape_seq.disable_bracketed_paste_seq;
    t.bracketed_paste_enabled <- false)

and handle_enable_focus_tracking t =
  if not t.focus_tracking_enabled then (
    write_output t Tty.Escape_seq.enable_focus_events_seq;
    t.focus_tracking_enabled <- true)

and handle_disable_focus_tracking t =
  if t.focus_tracking_enabled then (
    write_output t Tty.Escape_seq.disable_focus_events_seq;
    t.focus_tracking_enabled <- false)

and handle_set_window_title state title =
  (* OSC 2 ; title BEL *)
  write_output state (format "\x1b]2;%s\x07" title)

let init ~parent ~config ~tty =
  send parent (RendererStarted (self ()));
  let Config.{ render_mode; fps; output } = config in
  let _ =
    let after = Time.Duration.from_secs_float (fps_to_secs fps) in
    Timer.send_after (self ()) Tick ~after
  in
  let size = Tty.size tty in
  let matrix = Render.Matrix.create ~width:size.cols ~height:size.rows in
  loop
    {
      fps;
      runner = parent;
      tty;
      matrix;
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
      (* Initialize with empty element *)
      current_root_element = empty_element;
      frame_count = 0;
    }

let start ~config ~tty () =
  let parent = self () in
  let pid = spawn (fun () -> init ~parent ~config ~tty) in
  let selector msg =
    match msg with
    | RendererStarted pid' when Pid.equal pid pid' -> `select pid
    | _ -> `skip
  in
  let timeout = Time.Duration.from_secs 2 in
  receive ~selector ~timeout ()

let render pid element = send pid (Render element)
let resize pid ~width ~height = send pid (Resize { width; height })
let enter_alt_screen pid = send pid EnterAltScreen
let exit_alt_screen pid = send pid ExitAltScreen

let shutdown pid =
  send pid Shutdown;
  let selector msg =
    match msg with ShutdownComplete -> `select () | _ -> `skip
  in
  let timeout = Time.Duration.from_secs 2 in
  receive ~selector ~timeout ()

let hide_cursor pid = send pid (SetCursorVisibility `hidden)
let show_cursor pid = send pid (SetCursorVisibility `visible)
let enable_mouse pid mode = send pid (EnableMouse mode)
let disable_mouse pid = send pid DisableMouse
let enable_bracketed_paste pid = send pid EnableBracketedPaste
let disable_bracketed_paste pid = send pid DisableBracketedPaste
let enable_focus_tracking pid = send pid EnableFocusTracking
let disable_focus_tracking pid = send pid DisableFocusTracking
let set_window_title pid title = send pid (SetWindowTitle title)
