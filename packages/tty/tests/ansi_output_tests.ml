open Std

(** Helper to capture TTY output to a buffer *)
let capture_output f =
  let pipe = Kernel.Fd.pipe () in
  let tty = Tty.make 
    ~fd:pipe.write_fd 
    ~size:{rows=24; cols=80} 
    () 
    |> Result.unwrap
  in
  
  (* Execute the function *)
  f tty;
  
  (* Read the output *)
  let file = Fs.File.from_fd pipe.read_fd in
  let buffer = Bytes.create 1024 in
  match Fs.File.read file buffer ~offset:0 ~len:1024 with
  | Ok n -> Bytes.sub_string buffer 0 n
  | Error _ -> ""

(** Test cursor operations *)

let test_show_cursor () =
  let output = capture_output (fun tty -> Tty.show_cursor tty) in
  if output = "\x1b[?25h" then Ok ()
  else Error (format "Expected '\\x1b[?25h', got '%s'" output)

let test_hide_cursor () =
  let output = capture_output (fun tty -> Tty.hide_cursor tty) in
  if output = "\x1b[?25l" then Ok ()
  else Error (format "Expected '\\x1b[?25l', got '%s'" output)

let test_move_cursor () =
  let output = capture_output (fun tty -> Tty.move_cursor tty ~row:10 ~col:20) in
  if output = "\x1b[10;20H" then Ok ()
  else Error (format "Expected '\\x1b[10;20H', got '%s'" output)

let test_cursor_up () =
  let output = capture_output (fun tty -> Tty.cursor_up tty 5) in
  if output = "\x1b[5A" then Ok ()
  else Error (format "Expected '\\x1b[5A', got '%s'" output)

let test_cursor_down () =
  let output = capture_output (fun tty -> Tty.cursor_down tty 3) in
  if output = "\x1b[3B" then Ok ()
  else Error (format "Expected '\\x1b[3B', got '%s'" output)

let test_cursor_forward () =
  let output = capture_output (fun tty -> Tty.cursor_forward tty 7) in
  if output = "\x1b[7C" then Ok ()
  else Error (format "Expected '\\x1b[7C', got '%s'" output)

let test_cursor_back () =
  let output = capture_output (fun tty -> Tty.cursor_back tty 2) in
  if output = "\x1b[2D" then Ok ()
  else Error (format "Expected '\\x1b[2D', got '%s'" output)

(** Test screen operations *)

let test_clear () =
  let output = capture_output (fun tty -> Tty.clear tty) in
  if output = "\x1b[2J\x1b[1;1H" then Ok ()
  else Error (format "Expected '\\x1b[2J\\x1b[1;1H', got '%s'" output)

let test_clear_line () =
  let output = capture_output (fun tty -> Tty.clear_line tty) in
  if output = "\x1b[2K" then Ok ()
  else Error (format "Expected '\\x1b[2K', got '%s'" output)

let test_clear_to_end_of_line () =
  let output = capture_output (fun tty -> Tty.clear_to_end_of_line tty) in
  if output = "\x1b[0K" then Ok ()
  else Error (format "Expected '\\x1b[0K', got '%s'" output)

let test_clear_to_start_of_line () =
  let output = capture_output (fun tty -> Tty.clear_to_start_of_line tty) in
  if output = "\x1b[1K" then Ok ()
  else Error (format "Expected '\\x1b[1K', got '%s'" output)

(** Test alternate screen *)

let test_enter_alt_screen () =
  let output = capture_output (fun tty -> Tty.enter_alt_screen tty) in
  if output = "\x1b[?1049h" then Ok ()
  else Error (format "Expected '\\x1b[?1049h', got '%s'" output)

let test_exit_alt_screen () =
  let output = capture_output (fun tty -> Tty.exit_alt_screen tty) in
  if output = "\x1b[?1049l" then Ok ()
  else Error (format "Expected '\\x1b[?1049l', got '%s'" output)

(** Test mouse support *)

let test_enable_mouse_press () =
  let output = capture_output (fun tty -> 
    Tty.enable_mouse tty ~extended:false ~pixels:false Press
  ) in
  if output = "\x1b[?9h" then Ok ()
  else Error (format "Expected '\\x1b[?9h', got '%s'" output)

let test_enable_mouse_press_release () =
  let output = capture_output (fun tty -> 
    Tty.enable_mouse tty ~extended:false ~pixels:false PressAndRelease
  ) in
  if output = "\x1b[?1000h" then Ok ()
  else Error (format "Expected '\\x1b[?1000h', got '%s'" output)

let test_enable_mouse_with_extended () =
  let output = capture_output (fun tty -> 
    Tty.enable_mouse tty ~extended:true ~pixels:false Press
  ) in
  if output = "\x1b[?9h\x1b[?1006h" then Ok ()
  else Error (format "Expected '\\x1b[?9h\\x1b[?1006h', got '%s'" output)

let test_disable_mouse () =
  let output = capture_output (fun tty -> Tty.disable_mouse tty) in
  if output = "\x1b[?1000l\x1b[?1006l\x1b[?1016l" then Ok ()
  else Error (format "Expected mouse disable sequence, got '%s'" output)

(** Test enhanced features *)

let test_enable_bracketed_paste () =
  let output = capture_output (fun tty -> Tty.enable_bracketed_paste tty) in
  if output = "\x1b[?2004h" then Ok ()
  else Error (format "Expected '\\x1b[?2004h', got '%s'" output)

let test_disable_bracketed_paste () =
  let output = capture_output (fun tty -> Tty.disable_bracketed_paste tty) in
  if output = "\x1b[?2004l" then Ok ()
  else Error (format "Expected '\\x1b[?2004l', got '%s'" output)

let test_enable_focus_tracking () =
  let output = capture_output (fun tty -> Tty.enable_focus_tracking tty) in
  if output = "\x1b[?1004h" then Ok ()
  else Error (format "Expected '\\x1b[?1004h', got '%s'" output)

let test_disable_focus_tracking () =
  let output = capture_output (fun tty -> Tty.disable_focus_tracking tty) in
  if output = "\x1b[?1004l" then Ok ()
  else Error (format "Expected '\\x1b[?1004l', got '%s'" output)

let test_enable_kitty_keyboard () =
  let output = capture_output (fun tty -> Tty.enable_kitty_keyboard tty) in
  if output = "\x1b[>1u" then Ok ()
  else Error (format "Expected '\\x1b[>1u', got '%s'" output)

let test_disable_kitty_keyboard () =
  let output = capture_output (fun tty -> Tty.disable_kitty_keyboard tty) in
  if output = "\x1b[<u" then Ok ()
  else Error (format "Expected '\\x1b[<u', got '%s'" output)

let test_begin_sync () =
  let output = capture_output (fun tty -> Tty.begin_sync tty) in
  if output = "\x1b[?2026h" then Ok ()
  else Error (format "Expected '\\x1b[?2026h', got '%s'" output)

let test_end_sync () =
  let output = capture_output (fun tty -> Tty.end_sync tty) in
  if output = "\x1b[?2026l" then Ok ()
  else Error (format "Expected '\\x1b[?2026l', got '%s'" output)

(** Test multiple operations *)

let test_multiple_operations () =
  let output = capture_output (fun tty ->
    Tty.clear tty;
    Tty.move_cursor tty ~row:5 ~col:10;
    Tty.hide_cursor tty
  ) in
  let expected = "\x1b[2J\x1b[1;1H\x1b[5;10H\x1b[?25l" in
  if output = expected then Ok ()
  else Error (format "Multiple operations failed. Expected '%s', got '%s'" expected output)

let tests =
  let open Test in
  [
    case "show_cursor outputs correct ANSI" test_show_cursor;
    case "hide_cursor outputs correct ANSI" test_hide_cursor;
    case "move_cursor outputs correct ANSI" test_move_cursor;
    case "cursor_up outputs correct ANSI" test_cursor_up;
    case "cursor_down outputs correct ANSI" test_cursor_down;
    case "cursor_forward outputs correct ANSI" test_cursor_forward;
    case "cursor_back outputs correct ANSI" test_cursor_back;
    case "clear outputs correct ANSI" test_clear;
    case "clear_line outputs correct ANSI" test_clear_line;
    case "clear_to_end_of_line outputs correct ANSI" test_clear_to_end_of_line;
    case "clear_to_start_of_line outputs correct ANSI" test_clear_to_start_of_line;
    case "enter_alt_screen outputs correct ANSI" test_enter_alt_screen;
    case "exit_alt_screen outputs correct ANSI" test_exit_alt_screen;
    case "enable_mouse Press outputs correct ANSI" test_enable_mouse_press;
    case "enable_mouse PressAndRelease outputs correct ANSI" test_enable_mouse_press_release;
    case "enable_mouse with extended outputs correct ANSI" test_enable_mouse_with_extended;
    case "disable_mouse outputs correct ANSI" test_disable_mouse;
    case "enable_bracketed_paste outputs correct ANSI" test_enable_bracketed_paste;
    case "disable_bracketed_paste outputs correct ANSI" test_disable_bracketed_paste;
    case "enable_focus_tracking outputs correct ANSI" test_enable_focus_tracking;
    case "disable_focus_tracking outputs correct ANSI" test_disable_focus_tracking;
    case "enable_kitty_keyboard outputs correct ANSI" test_enable_kitty_keyboard;
    case "disable_kitty_keyboard outputs correct ANSI" test_disable_kitty_keyboard;
    case "begin_sync outputs correct ANSI" test_begin_sync;
    case "end_sync outputs correct ANSI" test_end_sync;
    case "multiple operations work correctly" test_multiple_operations;
  ]

let () =
  Miniriot.run
    ~main:(fun ~args -> Test.Cli.main ~name:"tty-ansi" ~tests ~args)
    ~args:Env.args ()
