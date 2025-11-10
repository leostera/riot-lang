open Std

(** Tests for ANSI escape sequences from the Escape_seq module *)

(** Test cursor operations *)

let test_show_cursor () =
  let output = Tty.Escape_seq.show_cursor_seq in
  if output = "\x1b[?25h" then Ok ()
  else Error ("Expected '\\x1b[?25h', got '" ^ output)

let test_hide_cursor () =
  let output = Tty.Escape_seq.hide_cursor_seq in
  if output = "\x1b[?25l" then Ok ()
  else Error ("Expected '\\x1b[?25l', got '" ^ output)

let test_cursor_position () =
  let output = Tty.Escape_seq.cursor_position_seq 10 20 in
  if output = "\x1b[10;20H" then Ok ()
  else Error ("Expected '\\x1b[10;20H', got '" ^ output)

let test_cursor_up () =
  let output = Tty.Escape_seq.cursor_up_seq 5 in
  if output = "\x1b[5A" then Ok ()
  else Error ("Expected '\\x1b[5A', got '" ^ output)

let test_cursor_down () =
  let output = Tty.Escape_seq.cursor_down_seq 3 in
  if output = "\x1b[3B" then Ok ()
  else Error ("Expected '\\x1b[3B', got '" ^ output)

let test_cursor_forward () =
  let output = Tty.Escape_seq.cursor_forward_seq 2 in
  if output = "\x1b[2C" then Ok ()
  else Error ("Expected '\\x1b[2C', got '" ^ output)

let test_cursor_back () =
  let output = Tty.Escape_seq.cursor_back_seq 4 in
  if output = "\x1b[4D" then Ok ()
  else Error ("Expected '\\x1b[4D', got '" ^ output)

(** Test screen operations *)

let test_erase_display () =
  let output = Tty.Escape_seq.erase_display_seq 2 in
  if output = "\x1b[2J" then Ok ()
  else Error ("Expected '\\x1b[2J', got '" ^ output)

let test_erase_line () =
  let output = Tty.Escape_seq.erase_entire_line_seq in
  if output = "\x1b[2K" then Ok ()
  else Error ("Expected '\\x1b[2K', got '" ^ output)

let test_erase_to_end_of_line () =
  let output = Tty.Escape_seq.erase_line_right_seq in
  if output = "\x1b[0K" then Ok ()
  else Error ("Expected '\\x1b[0K', got '" ^ output)

let test_erase_to_start_of_line () =
  let output = Tty.Escape_seq.erase_line_left_seq in
  if output = "\x1b[1K" then Ok ()
  else Error ("Expected '\\x1b[1K', got '" ^ output)

(** Test alt screen *)

let test_enter_alt_screen () =
  let output = Tty.Escape_seq.alt_screen_seq in
  if output = "\x1b[?1049h" then Ok ()
  else Error ("Expected '\\x1b[?1049h', got '" ^ output)

let test_exit_alt_screen () =
  let output = Tty.Escape_seq.exit_alt_screen_seq in
  if output = "\x1b[?1049l" then Ok ()
  else Error ("Expected '\\x1b[?1049l', got '" ^ output)

(** Test mouse operations *)

let test_enable_mouse_all_motion () =
  let output = Tty.Escape_seq.enable_mouse_all_motion_seq in
  if output = "\x1b[?1003h" then Ok ()
  else Error ("Expected '\\x1b[?1003h', got '" ^ output)

let test_disable_mouse () =
  let output = Tty.Escape_seq.disable_mouse_all_motion_seq in
  if output = "\x1b[?1003l" then Ok ()
  else Error ("Expected '\\x1b[?1003l', got '" ^ output)

(** Test bracketed paste *)

let test_enable_bracketed_paste () =
  let output = Tty.Escape_seq.enable_bracketed_paste_seq in
  if output = "\x1b[?2004h" then Ok ()
  else Error ("Expected '\\x1b[?2004h', got '" ^ output)

let test_disable_bracketed_paste () =
  let output = Tty.Escape_seq.disable_bracketed_paste_seq in
  if output = "\x1b[?2004l" then Ok ()
  else Error ("Expected '\\x1b[?2004l', got '" ^ output)

(** Test focus tracking *)

let test_enable_focus_tracking () =
  let output = Tty.Escape_seq.enable_focus_events_seq in
  if output = "\x1b[?1004h" then Ok ()
  else Error ("Expected '\\x1b[?1004h', got '" ^ output)

let test_disable_focus_tracking () =
  let output = Tty.Escape_seq.disable_focus_events_seq in
  if output = "\x1b[?1004l" then Ok ()
  else Error ("Expected '\\x1b[?1004l', got '" ^ output)

(** Run all tests *)

let () =
  let tests = [
    ("show_cursor", test_show_cursor);
    ("hide_cursor", test_hide_cursor);
    ("cursor_position", test_cursor_position);
    ("cursor_up", test_cursor_up);
    ("cursor_down", test_cursor_down);
    ("cursor_forward", test_cursor_forward);
    ("cursor_back", test_cursor_back);
    ("erase_display", test_erase_display);
    ("erase_line", test_erase_line);
    ("erase_to_end_of_line", test_erase_to_end_of_line);
    ("erase_to_start_of_line", test_erase_to_start_of_line);
    ("enter_alt_screen", test_enter_alt_screen);
    ("exit_alt_screen", test_exit_alt_screen);
    ("enable_mouse_all_motion", test_enable_mouse_all_motion);
    ("disable_mouse", test_disable_mouse);
    ("enable_bracketed_paste", test_enable_bracketed_paste);
    ("disable_bracketed_paste", test_disable_bracketed_paste);
    ("enable_focus_tracking", test_enable_focus_tracking);
    ("disable_focus_tracking", test_disable_focus_tracking);
  ] in
  
  let passed = cell 0 in
  let failed = cell 0 in
  
  List.iter (fun (name, test) ->
    match test () with
    | Ok () -> 
        print ("✓ " ^ name ^ "\n");
        passed := !passed + 1
    | Error msg -> 
        print ("✗ " ^ name ^ ": " ^ msg ^ "\n");
        failed := !failed + 1
  ) tests;
  
  print ("\n" ^ Int.to_string !passed ^ " passed, " ^ Int.to_string !failed ^ " failed\n");
  if !failed > 0 then exit 1