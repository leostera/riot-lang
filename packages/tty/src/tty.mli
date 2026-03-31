open Std

(** Terminal control and raw mode utilities.

    This module provides terminal state management and input reading.
    For escape sequences to control the terminal, use {!Escape_seq}.
    
    The TTY module is responsible for:
    - Managing terminal state (raw mode vs line-buffered)
    - Reading input from the terminal
    - Detecting terminal size
    
    It does NOT write output - all output should go to stdout using
    the escape sequences from {!Escape_seq}.

    {1 Example: Basic TTY Usage}

    {[
      open Tty

      (* Create TTY in raw mode *)
      let tty = match Tty.make_raw () with
        | Ok t -> t
        | Error _ -> panic "Failed to open TTY"
      in
      
      (* Get terminal size *)
      let size = Tty.size tty in
      Printf.printf "Terminal is %dx%d\n" size.cols size.rows;
      
      (* Read input *)
      match Tty.read tty with
      | Ok data -> Printf.printf "Read: %s\n" data
      | Error _ -> ()
      
      (* Restore terminal on exit *)
      Tty.restore tty
    ]} *)
(** {1 Modules} *)

module Escape_seq = Escape_seq

(** Re-export the Escape_seq module for pure ANSI escape sequences *)
module Color = Color

(** Re-export the Color module *)
module Profile = Profile

(** Re-export the Profile module *)
(** {1 Types} *)

type size = {
  rows : int;
  cols : int;
}
(** Terminal dimensions *)
type error =
  | NoTtyConnected
  | SystemError of IO.error
(** Error types for TTY operations *)
type mode =
  | LineBuffered
  | Immediate
(** Terminal input modes:
    - [LineBuffered]: Input is line-buffered (normal mode)
    - [Immediate]: Input is available immediately (raw/cbreak mode) *)
type t
(** Abstract terminal handle *)
(** {1 Creation and Management} *)

val make : ?fd:Kernel.Fd.t ->
?stdin:Kernel.Fd.t ->
?stdout:Kernel.Fd.t ->
?stderr:Kernel.Fd.t ->
?size:size ->
?mode:mode ->
unit ->
(t, error) result

(** [make ?fd ?stdin ?stdout ?stderr ?size ?mode ()] creates a new terminal handle.

    - [fd]: TTY file descriptor (defaults to opening /dev/tty)
    - [stdin]: Input file descriptor (defaults to Unix.stdin in non-blocking mode)
    - [stdout]: Output file descriptor (defaults to fd)
    - [stderr]: Error file descriptor (defaults to Unix.stderr)
    - [size]: Terminal size (auto-detected if not provided)
    - [mode]: Input mode (defaults to LineBuffered)
    
    {b Common usage:}
    
    Create a normal terminal (for line-based input):
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
      
      (* ... use tty for testing ... *)
    ]} *)
val make_raw : unit -> (t, error) result

(** [make_raw ()] creates a terminal in raw/cbreak mode.
    Convenience function equivalent to [make ~mode:Immediate ()].
    
    Creates terminal in raw/cbreak mode for TUI applications. *)
(** {1 Terminal Properties} *)

val size : t -> size

(** Get current terminal size. The size is cached and may not reflect
    real-time changes. Use {!refresh_size} to update. *)
val refresh_size : t -> unit

(** Refresh the cached terminal size by querying the terminal. *)
val mode : t -> mode

(** Get current terminal input mode. *)
val is_tty : Kernel.Fd.t -> bool

(** Check if a file descriptor is connected to a terminal. *)
(** {1 Terminal State} *)

val set_raw : t -> unit

(** Switch terminal to raw mode (immediate, non-echoed input). *)
val set_line_buffered : t -> unit

(** Switch terminal to line-buffered mode (normal terminal behavior). *)
val restore : t -> unit

(** Restore terminal to its original state when the TTY was created. *)
val suspend : t -> unit

(** Suspend terminal (SIGSTOP). Restores normal mode first if in raw mode. *)
val resume : t -> unit

(** Resume terminal after suspension, restoring previous mode. *)
(** {1 Input Operations} *)

type read =
  | Read of string
  | End
  | Malformed of string
  | Retry
(** Result type for low-level UTF-8 reading *)
val read_utf8 : t -> read

(** Low-level UTF-8 character reading. Used by io_loop for character-by-character input. *)
val read : t -> (string, IO.error) result

(** Read available input from the terminal.
    
    - In [LineBuffered] mode: blocks until a full line is available
    - In [Immediate] mode: returns immediately with available data
    
    The returned string may contain escape sequences for special keys. *)
val read_line : t -> (string, IO.error) result

(** Read a complete line from the terminal.
    Blocks until a newline is received. The returned string includes the newline. *)
(** {1 Utility} *)

val to_string : t -> string

(** Convert terminal to string for debugging. Displays terminal properties. *)
val equal : t -> t -> bool

(** Structural equality for terminal handles. *)
val stdin_fd : unit -> Kernel.Fd.t

(** Get the file descriptor for standard input (non-blocking). *)
val stdout_fd : unit -> Kernel.Fd.t

(** Get the file descriptor for standard output. *)
val stderr_fd : unit -> Kernel.Fd.t

(** Get the file descriptor for standard error. *)
module Style = Style

module Size = Size

module Input = Input

module Terminal_control = Terminal_control
