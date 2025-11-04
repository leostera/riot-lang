open Std
open Tty

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

type t = Pid.t

type state = {
  runner : Pid.t;
  ticker : Timer.id;
  width : int;
  height : int;
  render_mode : Config.render_mode;
  mutable buffer : string;
  mutable last_render : string;
  mutable lines_rendered : int;
  mutable is_altscreen_active : bool;
  mutable cursor_visibility : [ `hidden | `visible ];
  mutable mouse_enabled : bool;
  mutable mouse_mode : mouse_mode option;
  mutable bracketed_paste_enabled : bool;
  mutable focus_tracking_enabled : bool;
}

let is_empty state = String.length state.buffer = 0
let same_as_last_flush state = state.buffer = state.last_render
let lines state = state.buffer |> String.split_on_char '\n'

let rec loop state =
  match receive_any () with
  | Shutdown ->
      (* Make stdout blocking temporarily for shutdown to avoid Sys_blocked_io *)
      (try Unix.clear_nonblock Unix.stdout with _ -> ());
      
      (* Flush any pending buffer content first, without clearing previous lines *)
      (try 
        if not (is_empty state) then (
          (* Print the buffer directly without clearing, since this is the final output *)
          print "%s%!" state.buffer;
          state.buffer <- ""
        )
        with Sys_blocked_io -> ());
      (try restore state with Sys_blocked_io -> ());
      send state.runner ShutdownComplete
  | Tick ->
      tick state;
      loop state
  | Render output ->
      handle_render state output;
      loop state
  | Set_cursor_visibility cursor ->
      handle_set_cursor_visibility cursor state;
      loop state
  | Enter_alt_screen ->
      handle_enter_alt_screen state;
      loop state
  | Exit_alt_screen ->
      handle_exit_alt_screen state;
      loop state
  | Enable_mouse mode ->
      handle_enable_mouse state mode;
      loop state
  | Disable_mouse ->
      handle_disable_mouse state;
      loop state
  | Enable_bracketed_paste ->
      handle_enable_bracketed_paste state;
      loop state
  | Disable_bracketed_paste ->
      handle_disable_bracketed_paste state;
      loop state
  | Enable_focus_tracking ->
      handle_enable_focus_tracking state;
      loop state
  | Disable_focus_tracking ->
      handle_disable_focus_tracking state;
      loop state
  | Set_window_title title ->
      handle_set_window_title title;
      loop state
  | _ -> loop state

and restore t =
  let output = Buffer.create 64 in
  
  if t.cursor_visibility = `hidden then 
    Buffer.add_string output "\x1b[?25h"; (* show cursor *)
  
  if t.mouse_enabled then (
    (* Disable mouse SGR mode *)
    Buffer.add_string output "\x1b[?1006l";
    (* Disable mouse tracking *)
    match t.mouse_mode with
    | Some Cell_motion -> Buffer.add_string output "\x1b[?1002l"
    | Some All_motion -> Buffer.add_string output "\x1b[?1003l"
    | None -> ());
  
  if t.bracketed_paste_enabled then 
    Buffer.add_string output "\x1b[?2004l";
  
  if t.focus_tracking_enabled then 
    Buffer.add_string output "\x1b[?1004l";
  
  (* Single print with flush *)
  if Buffer.length output > 0 then
    print "%s%!" (Buffer.contents output)

and tick t =
  let now = Time.Instant.now () in
  if is_empty t || same_as_last_flush t then () else flush t;
  send t.runner (Io_loop.Input (Event.Frame now))

and flush t =
  let new_lines = lines t in
  let new_lines_this_flush = List.length new_lines in

  (* Build entire output as a string, then print once *)
  let output = Buffer.create 256 in
  
  (* Clean last rendered content *)
  if t.render_mode = Config.Clear then (
    if t.is_altscreen_active then
      (* In altscreen: move to home and clear entire screen *)
      Buffer.add_string output "\x1b[H\x1b[2J"
    else if t.lines_rendered > 0 then
      (* Normal mode: clear previous lines *)
      for _i = 1 to t.lines_rendered do
        Buffer.add_string output "\x1b[2K";  (* clear line *)
        Buffer.add_string output "\x1b[1A";  (* cursor up *)
      done
  );

  (* Add the actual buffer content *)
  Buffer.add_string output t.buffer;

  (* Move cursor back to start *)
  if t.is_altscreen_active then
    Buffer.add_string output (format "\x1b[%d;%dH" new_lines_this_flush 0)
  else
    Buffer.add_string output (format "\x1b[%dD" t.width);

  (* Single print with flush at the end *)
  print "%s%!" (Buffer.contents output);

  (* update state *)
  t.last_render <- t.buffer;
  t.lines_rendered <- new_lines_this_flush;
  t.buffer <- ""

and handle_render t output = t.buffer <- output

and handle_enter_alt_screen t =
  if t.is_altscreen_active then ()
  else (
    t.is_altscreen_active <- true;
    Terminal.enter_alt_screen ();
    (* Use explicit clear sequence for altscreen: home + clear entire screen *)
    print "\x1b[H\x1b[2J%!";
    t.last_render <- "")

and handle_exit_alt_screen t =
  if not t.is_altscreen_active then ()
  else (
    t.is_altscreen_active <- false;
    Terminal.exit_alt_screen ();
    t.last_render <- "")

and handle_set_cursor_visibility cursor t =
  if t.cursor_visibility = cursor then ()
  else (
    (match cursor with
    | `hidden -> Tty.Escape_seq.hide_cursor_seq ()
    | `visible -> Tty.Escape_seq.show_cursor_seq ());
    t.cursor_visibility <- cursor)

and handle_enable_mouse t mode =
  if not t.mouse_enabled then (
    (* Enable mouse SGR mode (1006) for better coordinate reporting *)
    print "\x1b[?1006h%!";
    (* Set the tracking mode *)
    (match mode with
    | Cell_motion ->
        (* Button event tracking + motion while button pressed (1002) *)
        print "\x1b[?1002h%!"
    | All_motion ->
        (* All motion tracking (1003) *)
        print "\x1b[?1003h%!");
    t.mouse_enabled <- true;
    t.mouse_mode <- Some mode)

and handle_disable_mouse t =
  if t.mouse_enabled then (
    (* Disable mouse SGR mode *)
    print "\x1b[?1006l%!";
    (* Disable mouse tracking *)
    (match t.mouse_mode with
    | Some Cell_motion -> print "\x1b[?1002l%!"
    | Some All_motion -> print "\x1b[?1003l%!"
    | None -> ());
    t.mouse_enabled <- false;
    t.mouse_mode <- None)

and handle_enable_bracketed_paste t =
  if not t.bracketed_paste_enabled then (
    print "\x1b[?2004h%!";
    t.bracketed_paste_enabled <- true)

and handle_disable_bracketed_paste t =
  if t.bracketed_paste_enabled then (
    print "\x1b[?2004l%!";
    t.bracketed_paste_enabled <- false)

and handle_enable_focus_tracking t =
  if not t.focus_tracking_enabled then (
    print "\x1b[?1004h%!";
    t.focus_tracking_enabled <- true)

and handle_disable_focus_tracking t =
  if t.focus_tracking_enabled then (
    print "\x1b[?1004l%!";
    t.focus_tracking_enabled <- false)

and handle_set_window_title title =
  (* OSC 2 ; title BEL *)
  print "\x1b]2;%s\x07%!" title

let max_fps = 120
let cap fps = Int.max 1 (Int.min fps max_fps) |> Int.to_float
let fps_to_secs fps = 1. /. cap fps

let start ~config () =
  let parent = self () in
  spawn (fun () ->
    send parent (RendererStarted (self ()));
    let Config.{ render_mode; fps } = config in
    let ticker =
      Timer.send_interval (self ()) Tick ~interval:(Time.Duration.from_secs_float (fps_to_secs fps))
    in
    (* Get actual terminal size *)
    let width, height = 
      match Tty.Terminal.size () with
      | Ok (w, h) -> (w, h)
      | Error _ -> (80, 24)  (* Fallback if not a TTY *)
    in
    loop
      {
        runner = parent;
        ticker;
        buffer = "";
        width;
        height;
        last_render = "";
        is_altscreen_active = false;
        lines_rendered = 0;
        cursor_visibility = `visible;
        render_mode;
        mouse_enabled = false;
        mouse_mode = None;
        bracketed_paste_enabled = false;
        focus_tracking_enabled = false;
      };
    Ok ()
  )

let render pid output = send pid (Render output)
let enter_alt_screen pid = send pid Enter_alt_screen
let exit_alt_screen pid = send pid Exit_alt_screen
let shutdown pid = send pid Shutdown
let hide_cursor pid = send pid (Set_cursor_visibility `hidden)
let show_cursor pid = send pid (Set_cursor_visibility `visible)
let enable_mouse pid mode = send pid (Enable_mouse mode)
let disable_mouse pid = send pid Disable_mouse
let enable_bracketed_paste pid = send pid Enable_bracketed_paste
let disable_bracketed_paste pid = send pid Disable_bracketed_paste
let enable_focus_tracking pid = send pid Enable_focus_tracking
let disable_focus_tracking pid = send pid Disable_focus_tracking
let set_window_title pid title = send pid (Set_window_title title)
