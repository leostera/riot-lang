(** TTY - Complete terminal control and input handling with testable design.
    
    All terminal operations take a {!t} handle, making it easy to:
    - Pass terminal configuration throughout your application
    - Create fake terminals for testing with custom buffers
    - Verify ANSI escape sequences in tests
    
    {1 Quick Start}
    
    {[
      open Std
      
      let () =
        match Tty.make ~mode:Immediate () with
        | Error _ -> eprintln "Not a terminal"
        | Ok tty ->
            Tty.enter_alt_screen tty;
            Tty.hide_cursor tty;
            
            let size = Tty.size tty in
            println "Terminal: %dx%d" size.cols size.rows;
            
            (* ... your TUI app ... *)
            
            Tty.show_cursor tty;
            Tty.exit_alt_screen tty;
            Tty.restore tty
    ]}
    
    {1 Testing with Fake TTY}
    
    {[
      open Std
      
      (* Create a pipe for capturing output *)
      let read_fd, write_fd = Pipe.create () in
      let tty = Tty.make 
        ~fd:write_fd 
        ~size:{rows=24; cols=80} 
        () 
        |> Result.expect
      in
      
      Tty.clear tty;
      
      (* Read the output from the pipe *)
      let file = Fs.File.from_fd read_fd in
      let buffer = Bytes.create 256 in
      match Fs.File.read file buffer ~offset:0 ~len:256 with
      | Ok n -> 
          let output = Bytes.sub_string buffer 0 n in
          assert (output = "\x1b[2J\x1b[1;1H")
      | Error _ -> ()
    ]} *)
open Std

type size = Terminal.size = {
  rows : int;  (** Terminal height in rows *)
  cols : int;  (** Terminal width in columns *)
}
(** Terminal dimensions *)

type error = Terminal.error =
  | NoTtyConnected              (** Not connected to a terminal *)
  | SystemError of Std.IO.error
(** Error types *)

type mode = Terminal.mode =
  | LineBuffered  (** Line-buffered, echoed input (default) *)
  | Immediate     (** Immediate, non-echoed input (cbreak mode) *)
(** Terminal mode *)

type t = Terminal.t
(** Terminal handle *)

val make : 
  ?fd:Kernel.Fd.t ->
  ?stdin:Kernel.Fd.t ->
  ?stdout:Kernel.Fd.t ->
  ?stderr:Kernel.Fd.t ->
  ?size:size ->
  ?mode:mode ->
  unit -> (t, error) result
(** Create a terminal handle with optional parameters.
    
    Parameters:
    - [fd]: File descriptor to use for termios operations (default: opens /dev/tty or stdin)
    - [stdin]: Input file descriptor (default: IO.stdin)
    - [stdout]: Output file descriptor (default: IO.stdout)
    - [stderr]: Error output file descriptor (default: IO.stderr)
    - [size]: Terminal dimensions (default: auto-detect or 80x24)
    - [mode]: Terminal mode (default: LineBuffered)
    
    Examples:
    
    Basic usage - auto-detect everything:
    {[
      match Tty.make () with
      | Ok tty -> (* ... *)
      | Error NoTtyConnected -> eprintln "Not a terminal"
    ]}
    
    Create in immediate mode:
    {[
      match Tty.make ~mode:Immediate () with
      | Ok tty -> (* immediate input *)
      | Error _ -> (* ... *)
    ]}
    
    Create fake TTY for testing with custom I/O:
    {[
      let input_read, input_write = Pipe.create () in
      let output_read, output_write = Pipe.create () in
      
      (* Write test input *)
      let file = Fs.File.from_fd input_write in
      Fs.File.write file (Bytes.of_string "hello") ~offset:0 ~len:5 |> ignore;
      
      (* Create TTY with custom stdin/stdout *)
      let tty = Tty.make 
        ~stdin:input_read
        ~stdout:output_write
        ~size:{rows=24; cols=80}
        () 
        |> Result.expect
      in
      
      (* Read input from the custom stdin *)
      match Tty.read_utf8 tty with
      | Read str -> assert (str = "h")
      | _ -> assert false
      
      (* Verify output written to custom stdout *)
      Tty.clear tty;
      let buffer = Bytes.create 256 in
      let out_file = Fs.File.from_fd output_read in
      match Fs.File.read out_file buffer ~offset:0 ~len:256 with
      | Ok n -> 
          let output = Bytes.sub_string buffer 0 n in
          assert (output = "\x1b[2J\x1b[1;1H")
      | Error _ -> ()
    ]} *)

val make_raw : unit -> (t, error) result
(** Convenience function: [make ~mode:Raw ()]
    
    Creates terminal in raw/cbreak mode for TUI applications. *)

(** {1 Terminal State Management} *)

val restore : t -> unit
(** Restore terminal to original state and close file descriptor. *)

val set_raw : t -> unit
(** Switch terminal to raw mode (immediate, non-echoed input). *)

val set_normal : t -> unit
(** Switch terminal to normal mode (line-buffered, echoed input). *)

val suspend : t -> unit
(** Suspend terminal (SIGSTOP). Restores normal mode first if in raw mode. *)

(** {1 Terminal Information} *)

val size : t -> size
(** Get current terminal dimensions. *)

val width : t -> int
(** Get terminal width in columns. *)

val height : t -> int
(** Get terminal height in rows. *)

val refresh_size : t -> unit
(** Re-detect terminal size and update cached value. *)

val fd : t -> Kernel.Fd.t
(** Get underlying file descriptor. *)

(** {1 Input} *)

type read = | Read of string | End | Malformed of string | Retry

val read_utf8 : t -> read 
(** Read a UTF-8 character from terminal.
    
    Returns:
    - [Read str] - Successfully read UTF-8 character
    - [End] - EOF reached
    - [Malformed msg] - Invalid UTF-8 sequence
    - [Retry] - Should retry (e.g., EINTR) *)

(** {1 Cursor Control} *)

val show_cursor : t -> unit
(** Make cursor visible. *)

val hide_cursor : t -> unit
(** Make cursor invisible. *)

val move_cursor : t -> row:int -> col:int -> unit
(** Move cursor to position (1-based). *)

val cursor_up : t -> int -> unit
(** Move cursor up n rows. *)

val cursor_down : t -> int -> unit
(** Move cursor down n rows. *)

val cursor_forward : t -> int -> unit
(** Move cursor forward (right) n columns. *)

val cursor_back : t -> int -> unit
(** Move cursor back (left) n columns. *)

(** {1 Screen Management} *)

val clear : t -> unit
(** Clear entire screen and move cursor to home (1,1). *)

val clear_line : t -> unit
(** Clear entire current line. *)

val clear_to_end_of_line : t -> unit
(** Clear from cursor to end of line. *)

val clear_to_start_of_line : t -> unit
(** Clear from cursor to start of line. *)

val enter_alt_screen : t -> unit
(** Enter alternate screen buffer. *)

val exit_alt_screen : t -> unit
(** Exit alternate screen buffer. *)

(** {1 Mouse Support} *)

type mouse_mode =
  | Press           (** Mouse button press only *)
  | PressAndRelease   (** Press and release events *)
  | CellMotion     (** Motion when button held *)
  | AllMotion      (** All motion events *)
(** Mouse tracking modes *)

val enable_mouse : t -> ?extended:bool -> ?pixels:bool -> mouse_mode -> unit
(** Enable mouse event tracking.
    
    - [extended]: Use SGR extended mode (supports larger terminals) - default true
    - [pixels]: Use pixel-level coordinates - default false *)

val disable_mouse : t -> unit
(** Disable all mouse tracking. *)

(** {1 Enhanced Features} *)

val enable_bracketed_paste : t -> unit
(** Enable bracketed paste mode (paste events wrapped with markers). *)

val disable_bracketed_paste : t -> unit
(** Disable bracketed paste mode. *)

val enable_focus_tracking : t -> unit
(** Enable focus in/out event tracking. *)

val disable_focus_tracking : t -> unit
(** Disable focus tracking. *)

val enable_kitty_keyboard : t -> unit
(** Enable Kitty keyboard protocol for enhanced key input. *)

val disable_kitty_keyboard : t -> unit
(** Disable Kitty keyboard protocol. *)

val begin_sync : t -> unit
(** Begin synchronized output (reduces flicker). *)

val end_sync : t -> unit
(** End synchronized output. *)

(** {1 Re-exported Modules} *)

module Color : module type of Color
module Escape_seq : module type of Escape_seq
module Profile : module type of Profile
module Style : module type of Style
module Size : module type of Size
module Input : module type of Input
module Terminal_control : module type of Terminal_control
