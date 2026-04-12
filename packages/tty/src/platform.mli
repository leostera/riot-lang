open Std

type fd = int
type termios
type when_to_apply =
  | Now
  | Drain
  | Flush
val stdin_fd: unit -> fd

val stdout_fd: unit -> fd

val stderr_fd: unit -> fd

val fd_equal: fd -> fd -> bool

val fd_to_int: fd -> int

val open_tty: unit -> (fd, Kernel.SystemError.t) result

val close: fd -> (unit, Kernel.SystemError.t) result

val is_tty: fd -> bool

val get_size: fd -> ((int * int), Kernel.SystemError.t) result

val get_attributes: fd -> (termios, Kernel.SystemError.t) result

val set_attributes: fd -> when_to_apply -> termios -> (unit, Kernel.SystemError.t) result

val make_raw_mode: termios -> termios

val default_termios: unit -> termios

val read: fd -> bytes -> offset:int -> len:int -> (int, Kernel.SystemError.t) result

val write: fd -> bytes -> offset:int -> len:int -> (int, Kernel.SystemError.t) result
