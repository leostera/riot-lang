(** Low-level terminal control primitives.
    
    This module wraps Unix terminal operations with meaningful names
    and provides a clean abstraction over termios. *)
open Global0

type termios = Unix.terminal_io

(* Keep the internal representation but make it abstract in the interface *)

type when_to_apply =
  | Now
  (** Apply immediately (was TCSANOW) *)
  | Drain
  (** Wait for output to finish (was TCSADRAIN) *)
  | Flush

(** Wait and discard pending input (was TCSAFLUSH) *)
let to_unix_when = function
  | Now -> Unix.TCSANOW
  | Drain -> Unix.TCSADRAIN
  | Flush -> Unix.TCSAFLUSH

let get_attributes = fun fd -> Unix.tcgetattr (Fd.to_unix fd)

let set_attributes = fun fd when_apply attrs ->
  Unix.tcsetattr (Fd.to_unix fd) (to_unix_when when_apply) attrs

let is_tty = fun fd -> Unix.isatty (Fd.to_unix fd)

external get_terminal_size: Unix.file_descr -> int * int = "caml_get_terminal_size"

let get_size = fun fd ->
  try
    let (cols, rows) = get_terminal_size (Fd.to_unix fd) in
    Ok (cols, rows)
  with
  | Failure msg -> Error (`System_error msg)
  | Unix.Unix_error (err, _, _) -> Error (`System_error (Unix.error_message err))

let make_raw_mode = fun termios ->
  Unix.{ termios with c_echo = false; c_icanon = false; c_icrnl = false }

let default_termios = fun () ->
  Unix.{
    c_ignbrk = false;
    c_brkint = false;
    c_ignpar = false;
    c_parmrk = false;
    c_inpck = false;
    c_istrip = false;
    c_inlcr = false;
    c_igncr = false;
    c_icrnl = false;
    c_ixon = false;
    c_ixoff = false;
    c_opost = false;
    c_obaud = 0;
    c_ibaud = 0;
    c_csize = 0;
    c_cstopb = 0;
    c_cread = false;
    c_parenb = false;
    c_parodd = false;
    c_hupcl = false;
    c_clocal = false;
    c_isig = false;
    c_icanon = false;
    c_noflsh = false;
    c_echo = false;
    c_echoe = false;
    c_echok = false;
    c_echonl = false;
    c_vintr = '\000';
    c_vquit = '\000';
    c_verase = '\000';
    c_vkill = '\000';
    c_veof = '\000';
    c_veol = '\000';
    c_vmin = 0;
    c_vtime = 0;
    c_vstart = '\000';
    c_vstop = '\000';
  }
