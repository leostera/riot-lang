(** Low-level terminal control primitives.
    
    This module wraps Unix terminal operations with meaningful names
    and provides a clean abstraction over termios. *)

open Global0

type termios = Unix.terminal_io

type when_to_apply =
  | Now     (** Apply immediately (was TCSANOW) *)
  | Drain   (** Wait for output to finish (was TCSADRAIN) *)
  | Flush   (** Wait and discard pending input (was TCSAFLUSH) *)

let to_unix_when = function
  | Now -> Unix.TCSANOW
  | Drain -> Unix.TCSADRAIN
  | Flush -> Unix.TCSAFLUSH

let get_attributes fd = Unix.tcgetattr (Fd.to_unix fd)

let set_attributes fd when_apply attrs =
  Unix.tcsetattr (Fd.to_unix fd) (to_unix_when when_apply) attrs

let is_tty fd = Unix.isatty (Fd.to_unix fd)

external get_terminal_size : Unix.file_descr -> int * int = "caml_get_terminal_size"

let get_size fd =
  try
    let (cols, rows) = get_terminal_size (Fd.to_unix fd) in
    Ok (cols, rows)
  with
  | Failure msg -> Error (`System_error msg)
  | Unix.Unix_error (err, _, _) -> Error (`System_error (Unix.error_message err))
