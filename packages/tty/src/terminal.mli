open Std

type size = {
  rows: int;
  cols: int;
}
type error =
  | NoTtyConnected
  | SystemError of IO.error
type mode =
  | LineBuffered
  | Immediate
type input_buffer = {
  data: bytes;
  mutable pos: int;
  mutable len: int;
}
type t = {
  fd: Platform.fd;
  owns_fd: bool;
  input_fd: Platform.fd;
  stdout: Platform.fd;
  stderr: Platform.fd;
  original_attrs: Platform.termios;
  mutable size: size;
  mutable mode: mode;
  mutable input_buffer: input_buffer option;
}
val write_to_fd: Platform.fd -> string -> unit

val write_escape: t -> string -> unit
