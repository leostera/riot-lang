(** Low-level terminal control primitives.
    
    This module wraps Unix terminal operations with meaningful names
    and provides a clean abstraction over termios. *)

type termios = Unix.terminal_io
(** Terminal I/O settings *)

type when_to_apply =
  | Now     (** Apply immediately (TCSANOW) *)
  | Drain   (** Wait for output to finish (TCSADRAIN) *)
  | Flush   (** Wait and discard pending input (TCSAFLUSH) *)
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

val get_size : Fd.t -> (int * int, [> `System_error of string]) result
(** Get terminal size as [(cols, rows)].
    
    Uses the [caml_get_terminal_size] C primitive via ioctl. *)
