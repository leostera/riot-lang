open Std

module Escape_seq = Escape_seq

module Color = Color

module Profile = Profile

module Style = Style

module Size = Size

module Input = Input

module Utf8_reader: sig
  type read_result =
  [
    `Ok of int
    | `Would_block
    | `Error
  ]
  type t
  type result =
  [
    `Retry
    | `End
    | `Malformed of string
    | `Read of string
  ]
  val create: unit -> t

  val read: t -> read:(bytes -> offset:int -> len:int -> read_result) -> result
end

module Terminal_control = Terminal_control

type fd
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
type t
val make:
  ?fd:fd -> ?stdin:fd -> ?stdout:fd -> ?stderr:fd -> ?size:size -> ?mode:mode -> unit -> (t, error) result

val make_raw: unit -> (t, error) result

val size: t -> size

val refresh_size: t -> unit

val mode: t -> mode

val is_tty: fd -> bool

val set_raw: t -> unit

val set_line_buffered: t -> unit

val restore: t -> unit

val suspend: t -> unit

val resume: t -> unit

type read =
  | Read of string
  | End
  | Malformed of string
  | Retry
val read_utf8: t -> read

val read: t -> (string, IO.error) result

val read_line: t -> (string, IO.error) result

val to_string: t -> string

val equal: t -> t -> bool

val stdin_fd: unit -> fd

val stdout_fd: unit -> fd

val stderr_fd: unit -> fd
