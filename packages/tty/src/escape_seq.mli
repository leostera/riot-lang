(** Low-level ANSI escape sequence primitives.

    This module provides the raw building blocks for terminal control via ANSI
    escape sequences. Most users should use higher-level modules like
    {!Terminal} or {!Style} instead.

    Functions ending in [_seq] print their escape sequence directly to stdout.
    String constants contain the escape sequences as values.

    {1 Example: Direct Sequence Usage}

    {[
      open Tty

      (* These print immediately *)
      Escape_seq.cursor_position_seq 10 20 ();
      Escape_seq.set_foreground_color_seq "255;0;0" ();
      print "Red text at position 10,20";
      print Escape_seq.reset_seq
    ]}

    {1 Example: Mouse Tracking}

    {[
      (* Enable mouse motion tracking *)
      Escape_seq.enable_mouse_all_motion_seq ();
      Escape_seq.enable_mouse_extended_mode_seq ();

      (* ... handle mouse events ... *)

      (* Disable when done *)
      Escape_seq.disable_mouse_all_motion_seq ();
      Escape_seq.disable_mouse_extended_mode_seq ()
    ]} *)

(** {1 Constants} *)

val csi : string
(** The Control Sequence Introducer: ["\x1b["] *)

val reset_seq : string
(** Sequence to reset all text attributes *)

val bold_seq : string
(** Sequence to enable bold text *)

val faint_seq : string
(** Sequence to enable faint/dim text *)

val italics_seq : string
(** Sequence to enable italic text *)

val underline_seq : string
(** Sequence to enable underlined text *)

val blink_seq : string
(** Sequence to enable blinking text *)

val reverse_seq : string
(** Sequence to enable reverse video (swap fg/bg colors) *)

val cross_out_seq : string
(** Sequence to enable crossed-out/strikethrough text *)

val overline_seq : string
(** Sequence to enable overlined text *)

val foreground_seq : string
(** Base sequence for setting foreground color *)

val background_seq : string
(** Base sequence for setting background color *)

(** {1 Screen Management} *)

val alt_screen_seq : unit -> unit
(** Print sequence to switch to alternate screen buffer *)

val exit_alt_screen_seq : unit -> unit
(** Print sequence to return to normal screen buffer *)

val save_screen_seq : unit -> unit
(** Print sequence to save current screen content *)

val restore_screen_seq : unit -> unit
(** Print sequence to restore previously saved screen *)

val erase_display_seq : int -> unit -> unit
(** [erase_display_seq mode] clears parts of the screen:
    - [0] = from cursor to end
    - [1] = from cursor to beginning
    - [2] = entire screen *)

val erase_line_seq : int -> unit -> unit
(** [erase_line_seq mode] clears parts of the line:
    - [0] = from cursor to end
    - [1] = from cursor to beginning
    - [2] = entire line *)

val erase_entire_line_seq : unit -> unit
(** Print sequence to clear the entire current line *)

val erase_line_left_seq : unit -> unit
(** Print sequence to clear from cursor to beginning of line *)

val erase_line_right_seq : unit -> unit
(** Print sequence to clear from cursor to end of line *)

(** {1 Cursor Control} *)

val cursor_position_seq : int -> int -> unit -> unit
(** [cursor_position_seq row col] moves cursor to position (1-based) *)

val cursor_up_seq : int -> unit -> unit
(** [cursor_up_seq n] moves cursor up [n] rows *)

val cursor_down_seq : int -> unit -> unit
(** [cursor_down_seq n] moves cursor down [n] rows *)

val cursor_forward_seq : int -> unit -> unit
(** [cursor_forward_seq n] moves cursor right [n] columns *)

val cursor_back_seq : int -> unit -> unit
(** [cursor_back_seq n] moves cursor left [n] columns *)

val cursor_next_line_seq : int -> unit -> unit
(** [cursor_next_line_seq n] moves cursor to beginning of line [n] rows down *)

val cursor_previous_line_seq : int -> unit -> unit
(** [cursor_previous_line_seq n] moves cursor to beginning of line [n] rows up
*)

val cursor_horizontal_seq : int -> unit -> unit
(** [cursor_horizontal_seq col] moves cursor to column [col] *)

val save_cursor_position_seq : unit -> unit
(** Print sequence to save current cursor position *)

val restore_cursor_position_seq : unit -> unit
(** Print sequence to restore previously saved cursor position *)

val show_cursor_seq : unit -> unit
(** Print sequence to make cursor visible *)

val hide_cursor_seq : unit -> unit
(** Print sequence to make cursor invisible *)

(** {1 Line Manipulation} *)

val insert_line_seq : int -> unit -> unit
(** [insert_line_seq n] inserts [n] blank lines at cursor *)

val delete_line_seq : int -> unit -> unit
(** [delete_line_seq n] deletes [n] lines at cursor *)

(** {1 Scrolling} *)

val scroll_up_seq : int -> unit -> unit
(** [scroll_up_seq n] scrolls screen up [n] lines *)

val scroll_down_seq : int -> unit -> unit
(** [scroll_down_seq n] scrolls screen down [n] lines *)

val change_scrolling_region_seq : int -> int -> unit -> unit
(** [change_scrolling_region_seq top bottom] sets scrolling region *)

(** {1 Colors} *)

val set_foreground_color_seq : string -> unit -> unit
(** [set_foreground_color_seq color] sets text color. [color] should be RGB like
    ["255;128;0"] *)

val set_background_color_seq : string -> unit -> unit
(** [set_background_color_seq color] sets background color. [color] should be
    RGB like ["255;128;0"] *)

val set_cursor_color_seq : string -> unit -> unit
(** [set_cursor_color_seq color] sets cursor color *)

(** {1 Window Control} *)

val set_window_title_seq : string -> unit -> unit
(** [set_window_title_seq title] sets terminal window title *)

(** {1 Mouse Tracking} *)

val enable_mouse_seq : unit -> unit
(** Enable basic mouse click tracking *)

val disable_mouse_seq : unit -> unit
(** Disable basic mouse click tracking *)

val enable_mouse_press_seq : unit -> unit
(** Enable mouse press event tracking *)

val disable_mouse_press_seq : unit -> unit
(** Disable mouse press event tracking *)

val enable_mouse_cell_motion_seq : unit -> unit
(** Enable mouse motion tracking (per cell) *)

val disable_mouse_cell_motion_seq : unit -> unit
(** Disable mouse motion tracking (per cell) *)

val enable_mouse_all_motion_seq : unit -> unit
(** Enable all mouse motion tracking *)

val disable_mouse_all_motion_seq : unit -> unit
(** Disable all mouse motion tracking *)

val enable_mouse_hilite_seq : unit -> unit
(** Enable mouse highlight tracking *)

val disable_mouse_hilite_seq : unit -> unit
(** Disable mouse highlight tracking *)

val enable_mouse_extended_mode_seq : unit -> unit
(** Enable extended mouse coordinate mode (supports larger terminals) *)

val disable_mouse_extended_mode_seq : unit -> unit
(** Disable extended mouse coordinate mode *)

val enable_mouse_pixels_mode_seq : unit -> unit
(** Enable pixel-level mouse tracking *)

val disable_mouse_pixels_mode_seq : unit -> unit
(** Disable pixel-level mouse tracking *)

(** {1 Bracketed Paste Mode} *)

val enable_bracketed_paste_seq : unit -> unit
(** Enable bracketed paste mode (paste events are bracketed with markers) *)

val disable_bracketed_paste_seq : unit -> unit
(** Disable bracketed paste mode *)

val start_bracketed_paste_seq : unit -> unit
(** Marker for start of paste *)

val end_bracketed_paste_seq : unit -> unit
(** Marker for end of paste *)

(** {1 Focus Tracking} *)

val enable_focus_events_seq : unit -> unit
(** Enable focus tracking (terminal will send events on focus in/out) *)

val disable_focus_events_seq : unit -> unit
(** Disable focus tracking *)

(** {1 Kitty Keyboard Protocol} *)

val enable_kitty_keyboard_seq : unit -> unit
(** Enable Kitty keyboard protocol for enhanced key input *)

val disable_kitty_keyboard_seq : unit -> unit
(** Disable Kitty keyboard protocol *)

(** {1 Synchronized Output} *)

val begin_sync_seq : unit -> unit
(** Begin synchronized output (reduces screen flicker) *)

val end_sync_seq : unit -> unit
(** End synchronized output *)

(** {1 String Utilities} *)

val strip : string -> string
(** [strip str] removes all ANSI escape sequences from [str].
    
    Returns the string with all ESC[ sequences removed, leaving only
    the visible text content.
    
    {[
      let colored = "\x1b[31mRed Text\x1b[0m" in
      strip colored  (* Returns "Red Text" *)
    ]} *)

val width : string -> int
(** [width str] calculates the display width of [str] ignoring ANSI codes.
    
    Returns the number of visible characters, not counting escape sequences.
    This is useful for text layout and alignment.
    
    {[
      let styled = "\x1b[1;32mBold Green\x1b[0m" in
      width styled  (* Returns 10, not 24 *)
    ]} *)
