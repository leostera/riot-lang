(** Low-level ANSI escape sequence primitives.

    This module provides pure functions that return ANSI escape sequence strings.
    These strings can be printed to stdout to control the terminal.
    
    Most users should use higher-level modules like {!Terminal} or {!Style} instead.

    {1 Example: Using Escape Sequences}

    {[
      open Tty

      (* Get escape sequences as strings *)
      let move_cursor = Escape_seq.cursor_position_seq 10 20 in
      let red_color = Escape_seq.set_foreground_color_seq "255;0;0" in
      let reset = Escape_seq.csi ^ Escape_seq.reset_seq ^ "m" in
      
      (* Print them to stdout *)
      print_string (move_cursor ^ red_color ^ "Red text at 10,20" ^ reset)
    ]}

    {1 Example: Mouse Tracking}

    {[
      (* Enable mouse motion tracking *)
      print_string Escape_seq.enable_mouse_all_motion_seq;
      print_string Escape_seq.enable_mouse_extended_mode_seq;

      (* ... handle mouse events ... *)

      (* Disable when done *)
      print_string Escape_seq.disable_mouse_all_motion_seq;
      print_string Escape_seq.disable_mouse_extended_mode_seq
    ]} *)

(** {1 Constants} *)

val csi : string
(** The Control Sequence Introducer: ["\x1b["] *)

val reset_seq : string
(** Code to reset all text attributes (use with CSI) *)

val bold_seq : string
(** Code to enable bold text (use with CSI) *)

val faint_seq : string
(** Code to enable faint/dim text (use with CSI) *)

val italics_seq : string
(** Code to enable italic text (use with CSI) *)

val underline_seq : string
(** Code to enable underlined text (use with CSI) *)

val blink_seq : string
(** Code to enable blinking text (use with CSI) *)

val reverse_seq : string
(** Code to enable reverse video (swap fg/bg colors) (use with CSI) *)

val cross_out_seq : string
(** Code to enable crossed-out/strikethrough text (use with CSI) *)

val overline_seq : string
(** Code to enable overlined text (use with CSI) *)

val foreground_seq : string
(** Base code for setting foreground color (use with CSI) *)

val background_seq : string
(** Base code for setting background color (use with CSI) *)

(** {1 Screen Management} *)

val alt_screen_seq : string
(** Sequence to switch to alternate screen buffer *)

val exit_alt_screen_seq : string
(** Sequence to return to normal screen buffer *)

val save_screen_seq : string
(** Sequence to save current screen content *)

val restore_screen_seq : string
(** Sequence to restore previously saved screen *)

val reset_scroll_region_seq : string
(** Sequence to reset scroll region to full screen *)

val erase_display_seq : int -> string
(** [erase_display_seq mode] returns sequence to clear parts of the screen:
    - [0] = from cursor to end
    - [1] = from cursor to beginning
    - [2] = entire screen *)

val erase_line_seq : int -> string
(** [erase_line_seq mode] returns sequence to clear parts of the line:
    - [0] = from cursor to end
    - [1] = from cursor to beginning
    - [2] = entire line *)

val erase_entire_line_seq : string
(** Sequence to clear the entire current line *)

val erase_line_left_seq : string
(** Sequence to clear from cursor to beginning of line *)

val erase_line_right_seq : string
(** Sequence to clear from cursor to end of line *)

(** {1 Cursor Control} *)

val cursor_position_seq : int -> int -> string
(** [cursor_position_seq row col] returns sequence to move cursor to position (1-based) *)

val cursor_up_seq : int -> string
(** [cursor_up_seq n] returns sequence to move cursor up [n] rows *)

val cursor_down_seq : int -> string
(** [cursor_down_seq n] returns sequence to move cursor down [n] rows *)

val cursor_forward_seq : int -> string
(** [cursor_forward_seq n] returns sequence to move cursor right [n] columns *)

val cursor_back_seq : int -> string
(** [cursor_back_seq n] returns sequence to move cursor left [n] columns *)

val cursor_next_line_seq : int -> string
(** [cursor_next_line_seq n] returns sequence to move cursor to beginning of line [n] rows down *)

val cursor_previous_line_seq : int -> string
(** [cursor_previous_line_seq n] returns sequence to move cursor to beginning of line [n] rows up *)

val cursor_horizontal_seq : int -> string
(** [cursor_horizontal_seq col] returns sequence to move cursor to column [col] *)

val save_cursor_position_seq : string
(** Sequence to save current cursor position *)

val restore_cursor_position_seq : string
(** Sequence to restore previously saved cursor position *)

val show_cursor_seq : string
(** Sequence to make cursor visible *)

val hide_cursor_seq : string
(** Sequence to make cursor invisible *)

(** {1 Line Manipulation} *)

val insert_line_seq : int -> string
(** [insert_line_seq n] returns sequence to insert [n] blank lines at cursor *)

val delete_line_seq : int -> string
(** [delete_line_seq n] returns sequence to delete [n] lines at cursor *)

(** {1 Scrolling} *)

val scroll_up_seq : int -> string
(** [scroll_up_seq n] returns sequence to scroll screen up [n] lines *)

val scroll_down_seq : int -> string
(** [scroll_down_seq n] returns sequence to scroll screen down [n] lines *)

val change_scrolling_region_seq : int -> int -> string
(** [change_scrolling_region_seq top bottom] returns sequence to set scrolling region *)

(** {1 Colors} *)

val set_foreground_color_seq : string -> string
(** [set_foreground_color_seq color] returns sequence to set text color. 
    [color] should be RGB like ["255;128;0"] *)

val set_background_color_seq : string -> string
(** [set_background_color_seq color] returns sequence to set background color. 
    [color] should be RGB like ["255;128;0"] *)

val set_cursor_color_seq : string -> string
(** [set_cursor_color_seq color] returns sequence to set cursor color *)

(** {1 Window Control} *)

val set_window_title_seq : string -> string
(** [set_window_title_seq title] returns sequence to set terminal window title *)

(** {1 Mouse Tracking} *)

val enable_mouse_seq : string
(** Sequence to enable basic mouse click tracking *)

val disable_mouse_seq : string
(** Sequence to disable basic mouse click tracking *)

val enable_mouse_press_seq : string
(** Sequence to enable mouse press event tracking *)

val disable_mouse_press_seq : string
(** Sequence to disable mouse press event tracking *)

val enable_mouse_cell_motion_seq : string
(** Sequence to enable mouse motion tracking (per cell) *)

val disable_mouse_cell_motion_seq : string
(** Sequence to disable mouse motion tracking (per cell) *)

val enable_mouse_all_motion_seq : string
(** Sequence to enable all mouse motion tracking *)

val disable_mouse_all_motion_seq : string
(** Sequence to disable all mouse motion tracking *)

val enable_mouse_hilite_seq : string
(** Sequence to enable mouse highlight tracking *)

val disable_mouse_hilite_seq : string
(** Sequence to disable mouse highlight tracking *)

val enable_mouse_extended_mode_seq : string
(** Sequence to enable extended mouse coordinate mode (supports larger terminals) *)

val disable_mouse_extended_mode_seq : string
(** Sequence to disable extended mouse coordinate mode *)

val enable_mouse_pixels_mode_seq : string
(** Sequence to enable pixel-level mouse tracking *)

val disable_mouse_pixels_mode_seq : string
(** Sequence to disable pixel-level mouse tracking *)

(** {1 Bracketed Paste Mode} *)

val enable_bracketed_paste_seq : string
(** Sequence to enable bracketed paste mode (paste events are bracketed with markers) *)

val disable_bracketed_paste_seq : string
(** Sequence to disable bracketed paste mode *)

val start_bracketed_paste_seq : string
(** Marker for start of paste *)

val end_bracketed_paste_seq : string
(** Marker for end of paste *)

(** {1 Focus Tracking} *)

val enable_focus_events_seq : string
(** Sequence to enable focus tracking (terminal will send events on focus in/out) *)

val disable_focus_events_seq : string
(** Sequence to disable focus tracking *)

(** {1 Kitty Keyboard Protocol} *)

val enable_kitty_keyboard_seq : string
(** Sequence to enable Kitty keyboard protocol for enhanced key input *)

val disable_kitty_keyboard_seq : string
(** Sequence to disable Kitty keyboard protocol *)

(** {1 Synchronized Output} *)

val begin_sync_seq : string
(** Sequence to begin synchronized output (reduces screen flicker) *)

val end_sync_seq : string
(** Sequence to end synchronized output *)

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