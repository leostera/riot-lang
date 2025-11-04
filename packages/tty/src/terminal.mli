(** High-level terminal control operations.

    This module provides convenient functions for common terminal operations
    like clearing the screen, moving the cursor, and managing alternate screen
    buffers. All functions in this module print ANSI escape sequences to stdout.

    {1 Example: Clearing and Positioning}

    {[
      open Std
      open Tty

      let () =
        Terminal.clear ();
        Terminal.move_cursor 5 10;
        println "Hello at row 5, column 10!"
    ]}

    {1 Example: Alternate Screen Buffer}

    The alternate screen buffer allows you to draw a full-screen interface
    without disturbing the user's scrollback:

    {[
      let show_fullscreen_ui () =
        Terminal.enter_alt_screen ();
        Terminal.clear ();

        (* Draw your UI *)
        Terminal.move_cursor 1 1;
        println "┌─────────────┐";
        Terminal.move_cursor 2 1;
        println "│  My App     │";
        Terminal.move_cursor 3 1;
        println "└─────────────┘";

        (* ... wait for user input ... *)

        (* Restore normal terminal *)
        Terminal.exit_alt_screen ()
    ]}

    {1 Example: Cursor Movement}

    {[
      let draw_menu items =
        List.iteri
          (fun i item ->
            Terminal.move_cursor (i + 1) 1;
            println (format "%d. %s" (i + 1) item))
          items;

        (* Move back up to first item *)
        Terminal.cursor_up (List.length items)
    ]} *)

val clear : unit -> unit
(** [clear ()] clears the entire screen and moves cursor to top-left (1,1).

    Equivalent to the 'clear' command. *)

val clear_line : unit -> unit
(** [clear_line ()] erases the entire current line without moving the cursor. *)

val cursor_down : int -> unit
(** [cursor_down n] moves the cursor down by [n] rows.

    The cursor stays in the same column. If already at bottom, has no effect. *)

val cursor_up : int -> unit
(** [cursor_up n] moves the cursor up by [n] rows.

    The cursor stays in the same column. If already at top, has no effect. *)

val cursor_back : int -> unit
(** [cursor_back n] moves the cursor back (left) by [n] columns.

    The cursor stays in the same row. If already at left edge, has no effect. *)

val enter_alt_screen : unit -> unit
(** [enter_alt_screen ()] switches to the alternate screen buffer.

    The alternate screen is a separate buffer that doesn't affect scrollback.
    Used for fullscreen applications. Always pair with {!exit_alt_screen}. *)

val exit_alt_screen : unit -> unit
(** [exit_alt_screen ()] returns to the normal screen buffer.

    Restores the terminal to the state before {!enter_alt_screen} was called. *)

val move_cursor : int -> int -> unit
(** [move_cursor row col] positions the cursor at the specified location.

    Row and column are 1-based (top-left is [1, 1]). *)

val size : unit -> (int * int, [ `System_error of string ]) result
(** [size ()] returns the current terminal dimensions as [(width, height)].

    Returns (columns, rows) for compatibility with common conventions.
    Returns Error if terminal size cannot be determined (e.g. not a TTY).
    
    Example:
    {[
      match Terminal.size () with
      | Ok (width, height) -> 
          Printf.printf "Terminal is %d×%d\n" width height
      | Error (`System_error msg) ->
          Printf.eprintf "Error: %s\n" msg
    ]} *)
