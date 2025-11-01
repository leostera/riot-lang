(** Advanced terminal control features.

    This module provides higher-level terminal control features beyond
    basic escape sequences, including synchronized updates for tear-free
    rendering, cursor styling, and line wrapping control.

    {1 Example: Synchronized Updates}

    {[
      open Std
      open Tty

      let render_frame buffer =
        Terminal_control.begin_synchronized_update ();
        (* All these writes are buffered and rendered atomically *)
        print_string (Escape_seq.csi);
        print_string "2J";  (* Clear screen *)
        print_string "Complex UI rendering here...";
        flush stdout;
        Terminal_control.end_synchronized_update ();
        (* Now the terminal updates all at once - no tearing! *)
    ]}

    {1 Example: Cursor Styling}

    {[
      (* Set a blinking bar cursor *)
      Terminal_control.set_cursor_style BlinkingBar;
      
      (* Steady block cursor for insert mode *)
      Terminal_control.set_cursor_style SteadyBlock;
    ]} *)

(** {1 Synchronized Updates} *)

val begin_synchronized_update : unit -> unit
(** [begin_synchronized_update ()] instructs the terminal to buffer updates.
    
    When synchronized update mode is enabled, the terminal continues to
    process escape sequences and text but delays rendering until
    {!end_synchronized_update} is called. This prevents tearing and
    flickering when updating complex UIs.
    
    Not all terminals support this feature. Unsupported terminals will
    ignore these sequences. *)

val end_synchronized_update : unit -> unit
(** [end_synchronized_update ()] flushes buffered updates to the screen.
    
    Must be paired with {!begin_synchronized_update}. *)

(** {1 Cursor Styling} *)

(** Cursor appearance styles *)
type cursor_style =
  | DefaultUserShape  (** Default cursor configured by user *)
  | BlinkingBlock  (** Blinking block cursor (■) *)
  | SteadyBlock  (** Steady block cursor *)
  | BlinkingUnderScore  (** Blinking underscore cursor (_) *)
  | SteadyUnderScore  (** Steady underscore cursor *)
  | BlinkingBar  (** Blinking bar cursor (|) *)
  | SteadyBar  (** Steady bar cursor *)

val set_cursor_style : cursor_style -> unit
(** [set_cursor_style style] changes the terminal cursor appearance.
    
    Not all terminals support all cursor styles. Unsupported styles
    may be ignored or fall back to a default. *)

(** {1 Line Wrapping} *)

val enable_line_wrap : unit -> unit
(** [enable_line_wrap ()] enables automatic line wrapping.
    
    When enabled (the default), text that exceeds the terminal width
    automatically continues on the next line. *)

val disable_line_wrap : unit -> unit
(** [disable_line_wrap ()] disables automatic line wrapping.
    
    When disabled, text that exceeds the terminal width is truncated.
    Useful for precise cursor positioning and full-screen applications. *)

(** {1 Window Size} *)

(** Window size information including pixel dimensions *)
type window_size = {
  rows : int;  (** Terminal height in rows *)
  columns : int;  (** Terminal width in columns *)
  width_px : int;  (** Terminal width in pixels (may be 0) *)
  height_px : int;  (** Terminal height in pixels (may be 0) *)
}

val window_size : unit -> window_size
(** [window_size ()] queries the terminal size.
    
    Returns both character-based dimensions (rows, columns) and
    pixel-based dimensions where available. Pixel dimensions may
    be 0 on platforms that don't support this query.
    
    Note: For basic size queries, use {!Size.get} which only returns
    rows and columns. *)

(** {1 Raw Mode Queries} *)

val is_raw_mode_enabled : unit -> bool
(** [is_raw_mode_enabled ()] checks if the terminal is in raw mode.
    
    Returns [true] if raw mode is currently enabled via {!Terminal.enter_raw_mode},
    [false] otherwise. This is useful for debugging or conditional behavior. *)
