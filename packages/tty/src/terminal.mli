open Std

(** Terminal dimensions *)
type size = {
  rows : int;
  cols : int;
}

(** Error types *)
type error = 
  | NoTtyConnected
  | SystemError of IO.error

(** Terminal mode *)
type mode = 
  | LineBuffered
  | Immediate

(** Terminal handle - internal representation *)
type t = {
  fd : Unix.file_descr;
  original_attrs : Kernel.Terminal.termios;
  mutable size : size;
  mutable mode : mode;
}

val write_to_fd : Unix.file_descr -> string -> unit
(** [write_to_fd fd str] writes a string to a file descriptor.
    Handles partial writes automatically. *)

val write_escape : t -> string -> unit
(** [write_escape t code] writes an ANSI escape sequence to the terminal.
    Automatically prepends the CSI sequence. *)
