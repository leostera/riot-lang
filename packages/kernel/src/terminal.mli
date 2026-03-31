(** Low-level terminal control primitives.
    
    This module wraps Unix terminal operations with meaningful names
    and provides a clean abstraction over termios. *)
open Global0

type termios
(** Terminal I/O settings - abstract wrapper around Unix.terminal_io *)
type when_to_apply =
  | Now
  (** Apply immediately (TCSANOW) *)
  | Drain
  (** Wait for output to finish (TCSADRAIN) *)
  | Flush
(** Wait and discard pending input (TCSAFLUSH) *)

(** When to apply terminal attribute changes *)
val get_attributes : Fd.t -> termios

(** Get current terminal attributes. Wraps [Unix.tcgetattr]. *)
val set_attributes : Fd.t -> when_to_apply -> termios -> unit

(** Set terminal attributes. Wraps [Unix.tcsetattr].
    
    @param fd File descriptor
    @param when When to apply changes
    @param attrs New terminal attributes *)
val is_tty : Fd.t -> bool

(** Check if file descriptor is a TTY. Wraps [Unix.isatty]. *)
val get_size : Fd.t -> (int * int, [>
  `System_error of string
]) result

(** Get terminal size as [(cols, rows)].
    
    Uses the [caml_get_terminal_size] C primitive via ioctl. *)
val make_raw_mode : termios -> termios

(** Create raw mode terminal settings from existing termios.
    Disables echo, canonical mode, and CR/NL mapping. *)
val default_termios : unit -> termios

(** Create a default termios structure with all flags disabled. *)
