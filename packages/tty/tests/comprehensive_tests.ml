open Std

(** # Comprehensive TTY Test Suite
    
    This suite thoroughly tests all TTY functionality including:
    - ANSI escape code correctness
    - Cursor operations
    - Screen management
    - Mouse support
    - Enhanced features
    - Edge cases and error handling
*)

(** Helper to capture TTY output to a buffer *)
let capture_output f =
  let pipe = Kernel.Fd.pipe () in
  let tty = Tty.make 
    ~stdout:pipe.write_fd 
    ~size:{rows=24; cols=80} 
    () 
    |> Result.unwrap
  in
  
  (* Execute the function *)
  f tty;
  
  (* Read the output *)
  let file = Fs.File.from_fd pipe.read_fd in
  let buffer = Bytes.create 2048 in
  match Fs.File.read file buffer ~offset:0 ~len:2048 with
  | Ok n -> Bytes.sub_string buffer 0 n
  | Error _ -> ""

(** Helper to create a fake TTY with custom size *)
let make_fake_tty ~rows ~cols =
  let pipe = Kernel.Fd.pipe () in
  Tty.make ~stdout:pipe.write_fd ~size:{rows; cols} () |> Result.unwrap

(** ## Cursor Visibility Tests *)

let test_show_cursor () =
  let output = capture_output (fun tty -> Tty.show_cursor tty) in
  if output = "\x1b[?25h" then Ok ()
  else Error (format "show_cursor: expected '\\x1b[?25h', got '%s'" output)

let test_hide_cursor () =
  let output = capture_output (fun tty -> Tty.hide_cursor tty) in
  if output = "\x1b[?25l" then Ok ()
  else Error (format "hide_cursor: expected '\\x1b[?25l', got '%s'" output)

let test_show_hide_sequence () =
  let output = capture_output (fun tty ->
    Tty.hide_cursor tty;
    Tty.show_cursor tty
  ) in
  if output = "\x1b[?25l\x1b[?25h" then Ok ()
  else Error (format "show/hide sequence failed, got '%s'" output)

(** ## Cursor Movement Tests *)

let test_move_cursor_home () =
  let output = capture_output (fun tty -> Tty.move_cursor tty ~row:1 ~col:1) in
  if output = "\x1b[1;1H" then Ok ()
  else Error (format "move to home: expected '\\x1b[1;1H', got '%s'" output)

let test_move_cursor_arbitrary () =
  let output = capture_output (fun tty -> Tty.move_cursor tty ~row:15 ~col:42) in
  if output = "\x1b[15;42H" then Ok ()
  else Error (format "move arbitrary: expected '\\x1b[15;42H', got '%s'" output)

let test_cursor_up_one () =
  let output = capture_output (fun tty -> Tty.cursor_up tty 1) in
  if output = "\x1b[1A" then Ok ()
  else Error (format "cursor up 1: expected '\\x1b[1A', got '%s'" output)

let test_cursor_up_multiple () =
  let output = capture_output (fun tty -> Tty.cursor_up tty 10) in
  if output = "\x1b[10A" then Ok ()
  else Error (format "cursor up 10: expected '\\x1b[10A', got '%s'" output)

let test_cursor_down_one () =
  let output = capture_output (fun tty -> Tty.cursor_down tty 1) in
  if output = "\x1b[1B" then Ok ()
  else Error (format "cursor down 1: expected '\\x1b[1B', got '%s'" output)

let test_cursor_down_multiple () =
  let output = capture_output (fun tty -> Tty.cursor_down tty 5) in
  if output = "\x1b[5B" then Ok ()
  else Error (format "cursor down 5: expected '\\x1b[5B', got '%s'" output)

let test_cursor_forward_one () =
  let output = capture_output (fun tty -> Tty.cursor_forward tty 1) in
  if output = "\x1b[1C" then Ok ()
  else Error (format "cursor forward 1: expected '\\x1b[1C', got '%s'" output)

let test_cursor_forward_multiple () =
  let output = capture_output (fun tty -> Tty.cursor_forward tty 20) in
  if output = "\x1b[20C" then Ok ()
  else Error (format "cursor forward 20: expected '\\x1b[20C', got '%s'" output)

let test_cursor_back_one () =
  let output = capture_output (fun tty -> Tty.cursor_back tty 1) in
  if output = "\x1b[1D" then Ok ()
  else Error (format "cursor back 1: expected '\\x1b[1D', got '%s'" output)

let test_cursor_back_multiple () =
  let output = capture_output (fun tty -> Tty.cursor_back tty 8) in
  if output = "\x1b[8D" then Ok ()
  else Error (format "cursor back 8: expected '\\x1b[8D', got '%s'" output)

(** ## Screen Clearing Tests *)

let test_clear_screen () =
  let output = capture_output (fun tty -> Tty.clear tty) in
  if output = "\x1b[2J\x1b[1;1H" then Ok ()
  else Error (format "clear screen: expected '\\x1b[2J\\x1b[1;1H', got '%s'" output)

let test_clear_line () =
  let output = capture_output (fun tty -> Tty.clear_line tty) in
  if output = "\x1b[2K" then Ok ()
  else Error (format "clear line: expected '\\x1b[2K', got '%s'" output)

let test_clear_to_end_of_line () =
  let output = capture_output (fun tty -> Tty.clear_to_end_of_line tty) in
  if output = "\x1b[0K" then Ok ()
  else Error (format "clear to EOL: expected '\\x1b[0K', got '%s'" output)

let test_clear_to_start_of_line () =
  let output = capture_output (fun tty -> Tty.clear_to_start_of_line tty) in
  if output = "\x1b[1K" then Ok ()
  else Error (format "clear to SOL: expected '\\x1b[1K', got '%s'" output)

(** ## Alternate Screen Tests *)

let test_enter_alt_screen () =
  let output = capture_output (fun tty -> Tty.enter_alt_screen tty) in
  if output = "\x1b[?1049h" then Ok ()
  else Error (format "enter alt: expected '\\x1b[?1049h', got '%s'" output)

let test_exit_alt_screen () =
  let output = capture_output (fun tty -> Tty.exit_alt_screen tty) in
  if output = "\x1b[?1049l" then Ok ()
  else Error (format "exit alt: expected '\\x1b[?1049l', got '%s'" output)

let test_alt_screen_round_trip () =
  let output = capture_output (fun tty ->
    Tty.enter_alt_screen tty;
    Tty.exit_alt_screen tty
  ) in
  if output = "\x1b[?1049h\x1b[?1049l" then Ok ()
  else Error (format "alt screen round trip failed, got '%s'" output)

(** ## Mouse Support Tests *)

let test_mouse_press_only () =
  let output = capture_output (fun tty ->
    Tty.enable_mouse tty ~extended:false ~pixels:false Press
  ) in
  if output = "\x1b[?9h" then Ok ()
  else Error (format "mouse press: expected '\\x1b[?9h', got '%s'" output)

let test_mouse_press_and_release () =
  let output = capture_output (fun tty ->
    Tty.enable_mouse tty ~extended:false ~pixels:false PressAndRelease
  ) in
  if output = "\x1b[?1000h" then Ok ()
  else Error (format "mouse press/release: expected '\\x1b[?1000h', got '%s'" output)

let test_mouse_cell_motion () =
  let output = capture_output (fun tty ->
    Tty.enable_mouse tty ~extended:false ~pixels:false CellMotion
  ) in
  if output = "\x1b[?1002h" then Ok ()
  else Error (format "mouse cell motion: expected '\\x1b[?1002h', got '%s'" output)

let test_mouse_all_motion () =
  let output = capture_output (fun tty ->
    Tty.enable_mouse tty ~extended:false ~pixels:false AllMotion
  ) in
  if output = "\x1b[?1003h" then Ok ()
  else Error (format "mouse all motion: expected '\\x1b[?1003h', got '%s'" output)

let test_mouse_with_extended_mode () =
  let output = capture_output (fun tty ->
    Tty.enable_mouse tty ~extended:true ~pixels:false Press
  ) in
  if output = "\x1b[?9h\x1b[?1006h" then Ok ()
  else Error (format "mouse + extended: expected '\\x1b[?9h\\x1b[?1006h', got '%s'" output)

let test_mouse_with_pixel_mode () =
  let output = capture_output (fun tty ->
    Tty.enable_mouse tty ~extended:false ~pixels:true Press
  ) in
  if output = "\x1b[?9h\x1b[?1016h" then Ok ()
  else Error (format "mouse + pixels: expected '\\x1b[?9h\\x1b[?1016h', got '%s'" output)

let test_mouse_with_all_modes () =
  let output = capture_output (fun tty ->
    Tty.enable_mouse tty ~extended:true ~pixels:true AllMotion
  ) in
  if output = "\x1b[?1003h\x1b[?1006h\x1b[?1016h" then Ok ()
  else Error (format "mouse all modes failed, got '%s'" output)

let test_disable_mouse () =
  let output = capture_output (fun tty -> Tty.disable_mouse tty) in
  if output = "\x1b[?1000l\x1b[?1006l\x1b[?1016l" then Ok ()
  else Error (format "disable mouse: expected all disable codes, got '%s'" output)

(** ## Bracketed Paste Tests *)

let test_enable_bracketed_paste () =
  let output = capture_output (fun tty -> Tty.enable_bracketed_paste tty) in
  if output = "\x1b[?2004h" then Ok ()
  else Error (format "enable paste: expected '\\x1b[?2004h', got '%s'" output)

let test_disable_bracketed_paste () =
  let output = capture_output (fun tty -> Tty.disable_bracketed_paste tty) in
  if output = "\x1b[?2004l" then Ok ()
  else Error (format "disable paste: expected '\\x1b[?2004l', got '%s'" output)

(** ## Focus Tracking Tests *)

let test_enable_focus_tracking () =
  let output = capture_output (fun tty -> Tty.enable_focus_tracking tty) in
  if output = "\x1b[?1004h" then Ok ()
  else Error (format "enable focus: expected '\\x1b[?1004h', got '%s'" output)

let test_disable_focus_tracking () =
  let output = capture_output (fun tty -> Tty.disable_focus_tracking tty) in
  if output = "\x1b[?1004l" then Ok ()
  else Error (format "disable focus: expected '\\x1b[?1004l', got '%s'" output)

(** ## Kitty Keyboard Protocol Tests *)

let test_enable_kitty_keyboard () =
  let output = capture_output (fun tty -> Tty.enable_kitty_keyboard tty) in
  if output = "\x1b[>1u" then Ok ()
  else Error (format "enable kitty: expected '\\x1b[>1u', got '%s'" output)

let test_disable_kitty_keyboard () =
  let output = capture_output (fun tty -> Tty.disable_kitty_keyboard tty) in
  if output = "\x1b[<u" then Ok ()
  else Error (format "disable kitty: expected '\\x1b[<u', got '%s'" output)

(** ## Synchronized Output Tests *)

let test_begin_sync () =
  let output = capture_output (fun tty -> Tty.begin_sync tty) in
  if output = "\x1b[?2026h" then Ok ()
  else Error (format "begin sync: expected '\\x1b[?2026h', got '%s'" output)

let test_end_sync () =
  let output = capture_output (fun tty -> Tty.end_sync tty) in
  if output = "\x1b[?2026l" then Ok ()
  else Error (format "end sync: expected '\\x1b[?2026l', got '%s'" output)

let test_sync_round_trip () =
  let output = capture_output (fun tty ->
    Tty.begin_sync tty;
    Tty.end_sync tty
  ) in
  if output = "\x1b[?2026h\x1b[?2026l" then Ok ()
  else Error (format "sync round trip failed, got '%s'" output)

(** ## Size and Dimension Tests *)

let test_size_accessor () =
  let tty = make_fake_tty ~rows:25 ~cols:100 in
  let size = Tty.size tty in
  if size.rows = 25 && size.cols = 100 then Ok ()
  else Error (format "size: expected 25x100, got %dx%d" size.rows size.cols)

let test_width_accessor () =
  let tty = make_fake_tty ~rows:30 ~cols:120 in
  let width = Tty.width tty in
  if width = 120 then Ok ()
  else Error (format "width: expected 120, got %d" width)

let test_height_accessor () =
  let tty = make_fake_tty ~rows:50 ~cols:80 in
  let height = Tty.height tty in
  if height = 50 then Ok ()
  else Error (format "height: expected 50, got %d" height)

let test_small_terminal () =
  let tty = make_fake_tty ~rows:10 ~cols:40 in
  let size = Tty.size tty in
  if size.rows = 10 && size.cols = 40 then Ok ()
  else Error "small terminal size incorrect"

let test_large_terminal () =
  let tty = make_fake_tty ~rows:100 ~cols:200 in
  let size = Tty.size tty in
  if size.rows = 100 && size.cols = 200 then Ok ()
  else Error "large terminal size incorrect"

(** ## Combined Operations Tests *)

let test_full_screen_setup () =
  let output = capture_output (fun tty ->
    Tty.enter_alt_screen tty;
    Tty.hide_cursor tty;
    Tty.clear tty
  ) in
  let expected = "\x1b[?1049h\x1b[?25l\x1b[2J\x1b[1;1H" in
  if output = expected then Ok ()
  else Error (format "full setup: expected '%s', got '%s'" expected output)

let test_full_screen_teardown () =
  let output = capture_output (fun tty ->
    Tty.show_cursor tty;
    Tty.exit_alt_screen tty
  ) in
  let expected = "\x1b[?25h\x1b[?1049l" in
  if output = expected then Ok ()
  else Error (format "full teardown: expected '%s', got '%s'" expected output)

let test_complex_cursor_dance () =
  let output = capture_output (fun tty ->
    Tty.move_cursor tty ~row:10 ~col:20;
    Tty.cursor_forward tty 5;
    Tty.cursor_down tty 2;
    Tty.cursor_back tty 3
  ) in
  let expected = "\x1b[10;20H\x1b[5C\x1b[2B\x1b[3D" in
  if output = expected then Ok ()
  else Error (format "cursor dance: expected '%s', got '%s'" expected output)

let test_enable_all_features () =
  let output = capture_output (fun tty ->
    Tty.enable_mouse tty AllMotion;
    Tty.enable_bracketed_paste tty;
    Tty.enable_focus_tracking tty;
    Tty.enable_kitty_keyboard tty
  ) in
  (* Mouse AllMotion with extended, then paste, focus, kitty *)
  let expected = "\x1b[?1003h\x1b[?1006h\x1b[?2004h\x1b[?1004h\x1b[>1u" in
  if output = expected then Ok ()
  else Error (format "enable all features failed, got '%s'" output)

let test_disable_all_features () =
  let output = capture_output (fun tty ->
    Tty.disable_mouse tty;
    Tty.disable_bracketed_paste tty;
    Tty.disable_focus_tracking tty;
    Tty.disable_kitty_keyboard tty
  ) in
  let expected = "\x1b[?1000l\x1b[?1006l\x1b[?1016l\x1b[?2004l\x1b[?1004l\x1b[<u" in
  if output = expected then Ok ()
  else Error (format "disable all features failed, got '%s'" output)

(** ## Mode Tests *)

let test_mode_enum_line_buffered () =
  let _ = Tty.LineBuffered in
  Ok ()

let test_mode_enum_immediate () =
  let _ = Tty.Immediate in
  Ok ()

(** ## Error Handling Tests *)

let test_error_no_tty_connected () =
  let _ = Tty.NoTtyConnected in
  Ok ()

let test_error_system_error () =
  let _ = Tty.SystemError (IO.Unknown_error "test") in
  Ok ()

(** ## Read Type Tests *)

let test_read_type_read () =
  let _ = Tty.Read "x" in
  Ok ()

let test_read_type_end () =
  let _ = Tty.End in
  Ok ()

let test_read_type_malformed () =
  let _ = Tty.Malformed "error" in
  Ok ()

let test_read_type_retry () =
  let _ = Tty.Retry in
  Ok ()

let tests =
  let open Test in
  [
    (* Cursor Visibility *)
    case "show_cursor outputs correct ANSI" test_show_cursor;
    case "hide_cursor outputs correct ANSI" test_hide_cursor;
    case "show/hide sequence works" test_show_hide_sequence;
    
    (* Cursor Movement *)
    case "move_cursor to home (1,1)" test_move_cursor_home;
    case "move_cursor to arbitrary position" test_move_cursor_arbitrary;
    case "cursor_up by 1" test_cursor_up_one;
    case "cursor_up by multiple" test_cursor_up_multiple;
    case "cursor_down by 1" test_cursor_down_one;
    case "cursor_down by multiple" test_cursor_down_multiple;
    case "cursor_forward by 1" test_cursor_forward_one;
    case "cursor_forward by multiple" test_cursor_forward_multiple;
    case "cursor_back by 1" test_cursor_back_one;
    case "cursor_back by multiple" test_cursor_back_multiple;
    
    (* Screen Clearing *)
    case "clear screen outputs correct ANSI" test_clear_screen;
    case "clear_line outputs correct ANSI" test_clear_line;
    case "clear_to_end_of_line outputs correct ANSI" test_clear_to_end_of_line;
    case "clear_to_start_of_line outputs correct ANSI" test_clear_to_start_of_line;
    
    (* Alternate Screen *)
    case "enter_alt_screen outputs correct ANSI" test_enter_alt_screen;
    case "exit_alt_screen outputs correct ANSI" test_exit_alt_screen;
    case "alt screen round trip works" test_alt_screen_round_trip;
    
    (* Mouse Support *)
    case "enable mouse Press mode" test_mouse_press_only;
    case "enable mouse PressAndRelease mode" test_mouse_press_and_release;
    case "enable mouse CellMotion mode" test_mouse_cell_motion;
    case "enable mouse AllMotion mode" test_mouse_all_motion;
    case "enable mouse with extended mode" test_mouse_with_extended_mode;
    case "enable mouse with pixel mode" test_mouse_with_pixel_mode;
    case "enable mouse with all modes" test_mouse_with_all_modes;
    case "disable_mouse outputs correct ANSI" test_disable_mouse;
    
    (* Bracketed Paste *)
    case "enable_bracketed_paste outputs correct ANSI" test_enable_bracketed_paste;
    case "disable_bracketed_paste outputs correct ANSI" test_disable_bracketed_paste;
    
    (* Focus Tracking *)
    case "enable_focus_tracking outputs correct ANSI" test_enable_focus_tracking;
    case "disable_focus_tracking outputs correct ANSI" test_disable_focus_tracking;
    
    (* Kitty Keyboard *)
    case "enable_kitty_keyboard outputs correct ANSI" test_enable_kitty_keyboard;
    case "disable_kitty_keyboard outputs correct ANSI" test_disable_kitty_keyboard;
    
    (* Synchronized Output *)
    case "begin_sync outputs correct ANSI" test_begin_sync;
    case "end_sync outputs correct ANSI" test_end_sync;
    case "sync round trip works" test_sync_round_trip;
    
    (* Size and Dimensions *)
    case "size() returns correct dimensions" test_size_accessor;
    case "width() returns correct value" test_width_accessor;
    case "height() returns correct value" test_height_accessor;
    case "small terminal dimensions work" test_small_terminal;
    case "large terminal dimensions work" test_large_terminal;
    
    (* Combined Operations *)
    case "full screen setup sequence" test_full_screen_setup;
    case "full screen teardown sequence" test_full_screen_teardown;
    case "complex cursor movement sequence" test_complex_cursor_dance;
    case "enable all features at once" test_enable_all_features;
    case "disable all features at once" test_disable_all_features;
    
    (* Mode Types *)
    case "LineBuffered mode exists" test_mode_enum_line_buffered;
    case "Immediate mode exists" test_mode_enum_immediate;
    
    (* Error Types *)
    case "NoTtyConnected error exists" test_error_no_tty_connected;
    case "SystemError error exists" test_error_system_error;
    
    (* Read Types *)
    case "Read variant exists" test_read_type_read;
    case "End variant exists" test_read_type_end;
    case "Malformed variant exists" test_read_type_malformed;
    case "Retry variant exists" test_read_type_retry;
  ]

let () =
  Miniriot.run
    ~main:(fun ~args -> Test.Cli.main ~name:"tty-comprehensive" ~tests ~args)
    ~args:Env.args ()
