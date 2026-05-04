# TTY - Terminal Control Library

A comprehensive library for terminal control, styling, and manipulation in OCaml.

## Features

- **Color Support** - RGB, ANSI, and ANSI256 colors
- **Text Styling** - Bold, italic, underline, blink, and more
- **Cursor Control** - Move, hide, show, save/restore position
- **Screen Management** - Clear, alternate screen, scrolling regions
- **Mouse Support** - Click, drag, motion tracking
- **Terminal Information** - Size detection, capability detection
- **Non-blocking Input** - Read stdin without blocking
- **Bracketed Paste** - Distinguish pasted text from typed text

## Quick Start

```ocaml
open Tty

(* Simple colored text *)
let () =
  print_string (Style.default |> Style.fg Color.red |> Style.bold |> Style.styled "Hello World!");
  print_newline ()

(* Terminal manipulation *)
let () =
  Terminal.clear () |> print_string;
  Terminal.move_cursor 10 5 |> print_string;
  print_string "Text at row 10, col 5";
  flush stdout

(* Get terminal size *)
let () =
  match Size.get () with
  | Ok size -> Printf.printf "Terminal: %dx%d\n" size.cols size.rows
  | Error _ -> print_endline "Could not get terminal size"
```

## Modules

### Color

Handle terminal colors with support for RGB, ANSI, and ANSI256 color modes.

```ocaml
(* Create colors *)
let red = Color.make "#FF0000"
let green = Color.from_rgb (0, 255, 0)
let blue = Color.ansi 4
let cyan = Color.ansi256 51

(* Check color types *)
if Color.is_rgb red then
  print_endline "Red is RGB color"

(* Convert to escape sequences *)
let fg_red = Color.to_escape_seq ~mode:`fg red
let bg_blue = Color.to_escape_seq ~mode:`bg blue
```

### Style

Combine colors and text attributes into reusable styles.

```ocaml
(* Build styles *)
let error_style = 
  Style.default
  |> Style.fg (Color.make "#FF0000")
  |> Style.bold

let warning_style =
  Style.default
  |> Style.fg (Color.make "#FFA500")
  |> Style.italic

let success_style =
  Style.default
  |> Style.fg (Color.make "#00FF00")
  |> Style.underline

(* Apply styles *)
print_endline (Style.styled error_style "Error: Something went wrong");
print_endline (Style.styled warning_style "Warning: Be careful");
print_endline (Style.styled success_style "Success: Operation completed");

(* Combine multiple attributes *)
let fancy =
  Style.default
  |> Style.fg (Color.make "#FF00FF")
  |> Style.bg (Color.make "#000000")
  |> Style.bold
  |> Style.italic
  |> Style.underline
```

### Terminal

Control terminal behavior and screen manipulation.

```ocaml
(* Clear operations *)
Terminal.clear () |> print_string;           (* Clear entire screen *)
Terminal.clear_line () |> print_string;      (* Clear current line *)

(* Cursor movement *)
Terminal.move_cursor 1 1 |> print_string;    (* Move to top-left *)
Terminal.cursor_up 5 |> print_string;        (* Move up 5 lines *)
Terminal.cursor_down 3 |> print_string;      (* Move down 3 lines *)
Terminal.cursor_back 10 |> print_string;     (* Move left 10 columns *)

(* Cursor visibility *)
Terminal.hide_cursor () |> print_string;
Terminal.show_cursor () |> print_string;

(* Save/restore cursor position *)
Terminal.save_cursor () |> print_string;
(* ... do some drawing ... *)
Terminal.restore_cursor () |> print_string;

(* Alternate screen *)
Terminal.enter_alt_screen () |> print_string;  (* Like vim/less *)
(* ... full screen app ... *)
Terminal.exit_alt_screen () |> print_string;

(* Scrolling regions *)
Terminal.set_scroll_region 5 20 |> print_string;  (* Lines 5-20 scroll *)
```

### Escape_seq

Low-level escape sequence generation.

```ocaml
(* Text attributes *)
print_string Escape_seq.bold_seq;
print_string "Bold text";
print_string Escape_seq.reset_seq;

print_string Escape_seq.italic_seq;
print_string "Italic text";
print_string Escape_seq.reset_seq;

(* Colors *)
print_string (Escape_seq.set_foreground_color_seq "255;0;0");  (* Red *)
print_string "Red text";
print_string Escape_seq.reset_seq;

(* Mouse support *)
Escape_seq.enable_mouse_seq () |> print_string;
Escape_seq.enable_mouse_cell_motion_seq () |> print_string;
(* ... handle mouse events ... *)
Escape_seq.disable_mouse_seq () |> print_string;

(* Bracketed paste *)
Escape_seq.enable_bracketed_paste_seq () |> print_string;
(* Pasted text will be wrapped in special sequences *)
Escape_seq.disable_bracketed_paste_seq () |> print_string;
```

### Stdin

Non-blocking stdin reading with UTF-8 support.

```ocaml
(* Setup non-blocking stdin *)
let old_settings = Stdin.setup () in

(* Read loop *)
let rec loop () =
  match Stdin.read_utf8 () with
  | `Read str -> 
      Printf.printf "Got: %s\n" str;
      if str = "q" then ()
      else loop ()
  | `Retry -> 
      Unix.sleepf 0.01;  (* No data yet *)
      loop ()
  | `End -> 
      print_endline "EOF"
  | `Malformed err ->
      Printf.printf "Malformed UTF-8: %s\n" err;
      loop ()
in

loop ();

(* Restore terminal *)
Stdin.shutdown old_settings
```

### Size

Get terminal dimensions.

```ocaml
match Size.get () with
| Ok { rows; cols } ->
    Printf.printf "Terminal is %d columns x %d rows\n" cols rows;
    
    (* Use for layout calculations *)
    let center_col = cols / 2 in
    let center_row = rows / 2 in
    Terminal.move_cursor center_row center_col |> print_string;
    print_string "Centered text";
    
| Error (`System_error msg) ->
    Printf.eprintf "Error getting terminal size: %s\n" msg
```

### Profile

Detect terminal color capabilities.

```ocaml
(* Detect best color profile *)
let profile = Profile.detect () in

match profile with
| Profile.TrueColor -> 
    print_endline "Terminal supports 24-bit RGB colors"
| Profile.ANSI256 -> 
    print_endline "Terminal supports 256 colors"
| Profile.ANSI -> 
    print_endline "Terminal supports basic 16 colors"
| Profile.Ascii -> 
    print_endline "Terminal does not support colors"
```

### Input

Parse terminal input events (keyboard, mouse, resize).

```ocaml
(* Setup *)
let old_settings = Stdin.setup () in
Input.enable_mouse () |> print_string;
Input.enable_bracketed_paste () |> print_string;

(* Parse input *)
let rec loop () =
  match Input.read () with
  | Some (`Key (key, modifiers)) ->
      Printf.printf "Key: %s, Modifiers: %s\n"
        (Input.Key.to_string key)
        (Input.Modifiers.to_string modifiers);
      loop ()
      
  | Some (`Mouse mouse) ->
      Printf.printf "Mouse: %s at (%d, %d)\n"
        (Input.Mouse.action_to_string mouse.action)
        mouse.x mouse.y;
      loop ()
      
  | Some (`Resize (cols, rows)) ->
      Printf.printf "Terminal resized to %dx%d\n" cols rows;
      loop ()
      
  | Some (`Paste text) ->
      Printf.printf "Pasted: %s\n" text;
      loop ()
      
  | None ->
      Unix.sleepf 0.01;
      loop ()
in

loop ();

(* Cleanup *)
Input.disable_mouse () |> print_string;
Input.disable_bracketed_paste () |> print_string;
Stdin.shutdown old_settings
```

## Complete Example: Simple TUI

```ocaml
open Tty

let draw_border cols rows =
  (* Top border *)
  Terminal.move_cursor 1 1 |> print_string;
  print_string "┌";
  for i = 2 to cols - 1 do print_string "─" done;
  print_string "┐";
  
  (* Side borders *)
  for row = 2 to rows - 1 do
    Terminal.move_cursor row 1 |> print_string;
    print_string "│";
    Terminal.move_cursor row cols |> print_string;
    print_string "│";
  done;
  
  (* Bottom border *)
  Terminal.move_cursor rows 1 |> print_string;
  print_string "└";
  for i = 2 to cols - 1 do print_string "─" done;
  print_string "┘"

let draw_title title cols =
  let title_len = String.length title in
  let start_col = (cols - title_len) / 2 in
  Terminal.move_cursor 1 start_col |> print_string;
  Style.default
  |> Style.bold
  |> Style.fg (Color.make "#00FFFF")
  |> Style.styled (" " ^ title ^ " ")
  |> print_string

let main () =
  (* Setup *)
  let old_settings = Stdin.setup () in
  Terminal.enter_alt_screen () |> print_string;
  Terminal.hide_cursor () |> print_string;
  Terminal.clear () |> print_string;
  
  (* Get size *)
  let size = match Size.get () with
    | Ok s -> s
    | Error _ -> { Size.rows = 24; cols = 80 }
  in
  
  (* Draw UI *)
  draw_border size.cols size.rows;
  draw_title "My TUI App" size.cols;
  
  (* Center message *)
  let msg = "Press 'q' to quit" in
  let msg_col = (size.cols - String.length msg) / 2 in
  let msg_row = size.rows / 2 in
  Terminal.move_cursor msg_row msg_col |> print_string;
  print_string msg;
  flush stdout;
  
  (* Event loop *)
  let rec loop () =
    match Stdin.read_utf8 () with
    | `Read "q" -> ()
    | `Read _ -> loop ()
    | `Retry -> Unix.sleepf 0.01; loop ()
    | `End -> ()
    | `Malformed _ -> loop ()
  in
  loop ();
  
  (* Cleanup *)
  Terminal.show_cursor () |> print_string;
  Terminal.exit_alt_screen () |> print_string;
  Stdin.shutdown old_settings

let () = main ()
```

## API Reference

See individual module documentation:
- [Color](src/color.mli) - Color handling
- [Style](src/style.mli) - Text styling
- [Terminal](src/terminal.ml) - Terminal control
- [Escape_seq](src/escape_seq.mli) - Escape sequences
- [Stdin](src/stdin.mli) - Input reading
- [Size](src/size.mli) - Terminal size
- [Profile](src/profile.mli) - Capability detection
- [Input](src/input.mli) - Event parsing

## Platform Support

- **Linux** - Full support
- **macOS** - Full support
- **Windows** - Partial support (Windows Terminal recommended)

## License

See project LICENSE file.
