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
(** Input buffer for efficient reading *)
type input_buffer = {
  data : bytes;  (** 4KB buffer *)
  mutable pos : int;  (** Current read position *)
  mutable len : int;  (** Valid data length *)
}
(** Input source configuration *)
type input_mode =
  | SingleFd of Kernel.Fd.t
  (** Traditional single FD mode *)
  | DualFd of {
      (** Dual FD mode for piped input + TTY control *)
      data_fd : Kernel.Fd.t;  (** stdin for data *)
      control_fd : Kernel.Fd.t;  (** /dev/tty for control *)
      mutable active :
        [
          `Data
          | `Control
        ];  (** Which FD to read from *)
    }
(** Terminal handle - internal representation *)
type t = {
  fd : Kernel.Fd.t;  (** Primary TTY fd - used for termios operations *)
  input : input_mode;  (** Input configuration *)
  stdout : Kernel.Fd.t;  (** Output file descriptor *)
  stderr : Kernel.Fd.t;  (** Error output file descriptor *)
  original_attrs : Kernel.Terminal.termios;
  mutable size : size;
  mutable mode : mode;
  mutable input_buffer : input_buffer option;  (** Buffered input *)
  mutable data_buffer : input_buffer option;  (** Separate buffer for data FD in dual mode *)
}
val write_to_fd : Kernel.Fd.t -> string -> unit

(** [write_to_fd fd str] writes a string to a file descriptor.
    Handles partial writes automatically. *)
val write_escape : t -> string -> unit

(** [write_escape t code] writes an ANSI escape sequence to the terminal.
    Automatically prepends the CSI sequence. *)
