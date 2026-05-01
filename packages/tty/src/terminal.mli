open Std

type size = { rows: int; cols: int }
type error =
  | NoTtyConnected
  | SystemError of IO.error
type mode =
  | LineBuffered
  | Immediate
type input_buffer = Utf8_reader.t
type t = {
  fd: Platform.fd;
  owns_fd: bool;
  input_fd: Platform.fd;
  stdout: Platform.fd;
  stderr: Platform.fd;
  original_attrs: Platform.termios;
  mutable size: size;
  mutable mode: mode;
  mutable resume_mode: mode option;
  input_buffer: input_buffer;
}

val write_to_fd: Platform.fd -> string -> unit

val write_escape: t -> string -> unit
