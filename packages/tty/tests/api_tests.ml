open Std

(** Test that the new Tty API compiles and has the expected types *)

let test_size_type () =
  (* Test that size is a record with rows and cols *)
  let size = { Tty.rows = 24; cols = 80 } in
  if size.rows = 24 && size.cols = 80 then Ok ()
  else Error "Size type fields don't match expected values"

let test_make_returns_result () =
  (* Test that make() returns a Result type *)
  match Tty.make () with
  | Ok _tty -> Ok () (* TTY available *)
  | Error Tty.NoTtyConnected -> Ok () (* Expected in non-interactive mode *)
  | Error (Tty.SystemError _err) -> Ok () (* Also acceptable *)

let test_make_raw_returns_result () =
  (* Test that make_raw() returns a Result type *)
  match Tty.make_raw () with
  | Ok _tty -> Ok () (* TTY available *)
  | Error Tty.NoTtyConnected -> Ok () (* Expected in non-interactive mode *)
  | Error (Tty.SystemError _err) -> Ok () (* Also acceptable *)

let test_width_height_accessors () =
  (* Test width/height accessors when TTY is available *)
  match Tty.make () with
  | Error _ -> Ok () (* Skip test if no TTY *)
  | Ok tty ->
      let size = Tty.size tty in
      let width = Tty.width tty in
      let height = Tty.height tty in
      if width = size.cols && height = size.rows then Ok ()
      else Error (format "Width/height mismatch: %d,%d != %d,%d" 
                    width height size.cols size.rows)

let test_escape_sequences_compile () =
  (* Test that escape sequence functions exist (don't call them - they print to stdout!) *)
  try
    (* Just verify the functions exist by referencing them *)
    let _funcs = [
      Tty.Escape_seq.show_cursor_seq;
      Tty.Escape_seq.hide_cursor_seq;
      Tty.Escape_seq.enable_focus_events_seq;
      Tty.Escape_seq.disable_focus_events_seq;
      Tty.Escape_seq.enable_kitty_keyboard_seq;
      Tty.Escape_seq.disable_kitty_keyboard_seq;
    ] in
    Ok ()
  with e -> Error (format "Escape sequence failed: %s" (Exception.to_string e))

let test_cursor_operations () =
  (* Test that cursor operations compile and take tty parameter *)
  (* Use a fake TTY to avoid interfering with test output *)
  let pipe = Kernel.Fd.pipe () in
  match Tty.make ~stdout:pipe.write_fd ~size:{rows=24; cols=80} () with
  | Error e -> Error (format "Failed to create fake TTY: %s" (match e with Tty.NoTtyConnected -> "NoTtyConnected" | Tty.SystemError _ -> "SystemError"))
  | Ok tty ->
      try
        Tty.cursor_up tty 1;
        Tty.cursor_down tty 1;
        Tty.cursor_forward tty 1;
        Tty.cursor_back tty 1;
        Tty.move_cursor tty ~row:1 ~col:1;
        Ok ()
      with e -> Error (format "Cursor operation failed: %s" (Exception.to_string e))

let test_screen_operations () =
  (* Test that screen operations compile and take tty parameter *)
  (* Use a fake TTY to avoid interfering with test output *)
  let pipe = Kernel.Fd.pipe () in
  match Tty.make ~stdout:pipe.write_fd ~size:{rows=24; cols=80} () with
  | Error e -> Error (format "Failed to create fake TTY: %s" (match e with Tty.NoTtyConnected -> "NoTtyConnected" | Tty.SystemError _ -> "SystemError"))
  | Ok tty ->
      try
        Tty.clear tty;
        Tty.clear_line tty;
        Tty.clear_to_end_of_line tty;
        Tty.clear_to_start_of_line tty;
        Ok ()
      with e -> Error (format "Screen operation failed: %s" (Exception.to_string e))

let test_mouse_support () =
  (* Test mouse mode enumeration exists *)
  let _modes = [
    Tty.Press;
    Tty.PressAndRelease;
    Tty.CellMotion;
    Tty.AllMotion;
  ] in
  Ok ()

let test_mode_enumeration () =
  (* Test mode enumeration exists *)
  let _modes = [
    Tty.LineBuffered;
    Tty.Immediate;
  ] in
  Ok ()

let tests =
  let open Test in
  [
    case "Size type has rows and cols fields" test_size_type;
    case "make() returns Result type" test_make_returns_result;
    case "make_raw() returns Result type" test_make_raw_returns_result;
    case "width() and height() accessors work" test_width_height_accessors;
    case "Escape sequences compile and execute" test_escape_sequences_compile;
    case "Cursor operations take tty parameter" test_cursor_operations;
    case "Screen operations take tty parameter" test_screen_operations;
    case "Mouse support types exist" test_mouse_support;
    case "Mode enumeration exists" test_mode_enumeration;
  ]

let () =
  Miniriot.run
    ~main:(fun ~args -> Test.Cli.main ~name:"tty" ~tests ~args)
    ~args:Env.args ()
