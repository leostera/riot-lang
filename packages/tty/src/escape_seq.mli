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

(** The Control Sequence Introducer: ["\x1b["] *)
val csi : string

(** Code to reset all text attributes (use with CSI) *)
val reset_seq : string

(** Code to enable bold text (use with CSI) *)
val bold_seq : string

(** Code to enable faint/dim text (use with CSI) *)
val faint_seq : string

(** Code to enable italic text (use with CSI) *)
val italics_seq : string

(** Code to enable underlined text (use with CSI) *)
val underline_seq : string

(** Code to enable blinking text (use with CSI) *)
val blink_seq : string

(** Code to enable reverse video (swap fg/bg colors) (use with CSI) *)
val reverse_seq : string

(** Code to enable crossed-out/strikethrough text (use with CSI) *)
val cross_out_seq : string

(** Code to enable overlined text (use with CSI) *)
val overline_seq : string

(** Base code for setting foreground color (use with CSI) *)
val foreground_seq : string

(** Base code for setting background color (use with CSI) *)
val background_seq : string

(** {1 Screen Management} *)

(** Sequence to switch to alternate screen buffer *)
val alt_screen_seq : string

(** Sequence to return to normal screen buffer *)
val exit_alt_screen_seq : string

(** Sequence to save current screen content *)
val save_screen_seq : string

(** Sequence to restore previously saved screen *)
val restore_screen_seq : string

(** Sequence to reset scroll region to full screen *)
val reset_scroll_region_seq : string

(** [erase_display_seq mode] returns sequence to clear parts of the screen:
    - [0] = from cursor to end
    - [1] = from cursor to beginning
    - [2] = entire screen *)
val erase_display_seq : int -> string

(** [erase_line_seq mode] returns sequence to clear parts of the line:
    - [0] = from cursor to end
    - [1] = from cursor to beginning
    - [2] = entire line *)
val erase_line_seq : int -> string

(** Sequence to clear the entire current line *)
val erase_entire_line_seq : string

(** Sequence to clear from cursor to beginning of line *)
val erase_line_left_seq : string

(** Sequence to clear from cursor to end of line *)
val erase_line_right_seq : string

(** {1 Cursor Control} *)

(** [cursor_position_seq row col] returns sequence to move cursor to position (1-based) *)
val cursor_position_seq : int -> int -> string

(** [cursor_up_seq n] returns sequence to move cursor up [n] rows *)
val cursor_up_seq : int -> string

(** [cursor_down_seq n] returns sequence to move cursor down [n] rows *)
val cursor_down_seq : int -> string

(** [cursor_forward_seq n] returns sequence to move cursor right [n] columns *)
val cursor_forward_seq : int -> string

(** [cursor_back_seq n] returns sequence to move cursor left [n] columns *)
val cursor_back_seq : int -> string

(** [cursor_next_line_seq n] returns sequence to move cursor to beginning of line [n] rows down *)
val cursor_next_line_seq : int -> string

(** [cursor_previous_line_seq n] returns sequence to move cursor to beginning of line [n] rows up *)
val cursor_previous_line_seq : int -> string

(** [cursor_horizontal_seq col] returns sequence to move cursor to column [col] *)
val cursor_horizontal_seq : int -> string

(** Sequence to save current cursor position *)
val save_cursor_position_seq : string

(** Sequence to restore previously saved cursor position *)
val restore_cursor_position_seq : string

(** Sequence to make cursor visible *)
val show_cursor_seq : string

(** Sequence to make cursor invisible *)
val hide_cursor_seq : string

(** {1 Line Manipulation} *)

(** [insert_line_seq n] returns sequence to insert [n] blank lines at cursor *)
val insert_line_seq : int -> string

(** [delete_line_seq n] returns sequence to delete [n] lines at cursor *)
val delete_line_seq : int -> string

(** {1 Scrolling} *)

(** [scroll_up_seq n] returns sequence to scroll screen up [n] lines *)
val scroll_up_seq : int -> string

(** [scroll_down_seq n] returns sequence to scroll screen down [n] lines *)
val scroll_down_seq : int -> string

(** [change_scrolling_region_seq top bottom] returns sequence to set scrolling region *)
val change_scrolling_region_seq : int -> int -> string

(** {1 Colors} *)

(** [set_foreground_color_seq color] returns sequence to set text color. 
    [color] should be RGB like ["255;128;0"] *)
val set_foreground_color_seq : string -> string

(** [set_background_color_seq color] returns sequence to set background color. 
    [color] should be RGB like ["255;128;0"] *)
val set_background_color_seq : string -> string

(** [set_cursor_color_seq color] returns sequence to set cursor color *)
val set_cursor_color_seq : string -> string

(** {1 Window Control} *)

(** [set_window_title_seq title] returns sequence to set terminal window title *)
val set_window_title_seq : string -> string

(** {1 Mouse Tracking} *)

(** Sequence to enable basic mouse click tracking *)
val enable_mouse_seq : string

(** Sequence to disable basic mouse click tracking *)
val disable_mouse_seq : string

(** Sequence to enable mouse press event tracking *)
val enable_mouse_press_seq : string

(** Sequence to disable mouse press event tracking *)
val disable_mouse_press_seq : string

(** Sequence to enable mouse motion tracking (per cell) *)
val enable_mouse_cell_motion_seq : string

(** Sequence to disable mouse motion tracking (per cell) *)
val disable_mouse_cell_motion_seq : string

(** Sequence to enable all mouse motion tracking *)
val enable_mouse_all_motion_seq : string

(** Sequence to disable all mouse motion tracking *)
val disable_mouse_all_motion_seq : string

(** Sequence to enable mouse highlight tracking *)
val enable_mouse_hilite_seq : string

(** Sequence to disable mouse highlight tracking *)
val disable_mouse_hilite_seq : string

(** Sequence to enable extended mouse coordinate mode (supports larger terminals) *)
val enable_mouse_extended_mode_seq : string

(** Sequence to disable extended mouse coordinate mode *)
val disable_mouse_extended_mode_seq : string

(** Sequence to enable pixel-level mouse tracking *)
val enable_mouse_pixels_mode_seq : string

(** Sequence to disable pixel-level mouse tracking *)
val disable_mouse_pixels_mode_seq : string

(** {1 Bracketed Paste Mode} *)

(** Sequence to enable bracketed paste mode (paste events are bracketed with markers) *)
val enable_bracketed_paste_seq : string

(** Sequence to disable bracketed paste mode *)
val disable_bracketed_paste_seq : string

(** Marker for start of paste *)
val start_bracketed_paste_seq : string

(** Marker for end of paste *)
val end_bracketed_paste_seq : string

(** {1 Focus Tracking} *)

(** Sequence to enable focus tracking (terminal will send events on focus in/out) *)
val enable_focus_events_seq : string

(** Sequence to disable focus tracking *)
val disable_focus_events_seq : string

(** {1 Kitty Keyboard Protocol} *)

(** Sequence to enable Kitty keyboard protocol for enhanced key input *)
val enable_kitty_keyboard_seq : string

(** Sequence to disable Kitty keyboard protocol *)
val disable_kitty_keyboard_seq : string

(** {1 Synchronized Output} *)

(** Sequence to begin synchronized output (reduces screen flicker) *)
val begin_sync_seq : string

(** Sequence to end synchronized output *)
val end_sync_seq : string

(** {1 String Utilities} *)

(** [strip str] removes all ANSI escape sequences from [str].
    
    Returns the string with all ESC[ sequences removed, leaving only
    the visible text content.
    
    {[
      let colored = "\x1b[31mRed Text\x1b[0m" in
      strip colored  (* Returns "Red Text" *)
    ]} *)
val strip : string -> string

(** [width str] calculates the display width of [str] ignoring ANSI codes.
    
    Returns the number of visible characters, not counting escape sequences.
    This is useful for text layout and alignment.
    
    {[
      let styled = "\x1b[1;32mBold Green\x1b[0m" in
      width styled  (* Returns 10, not 24 *)
    ]} *)
val width : string -> int