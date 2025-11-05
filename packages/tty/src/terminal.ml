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

(** Terminal handle *)
type t = {
  fd : Unix.file_descr;
  original_attrs : Kernel.Terminal.termios;
  mutable size : size;
  mutable mode : mode;
}

(* Helper to write to file descriptor *)
let write_to_fd fd str =
  let bytes = Bytes.of_string str in
  let len = String.length str in
  let rec write_loop offset remaining =
    if remaining = 0 then ()
    else
      let written = Unix.write fd bytes offset remaining in
      write_loop (offset + written) (remaining - written)
  in
  write_loop 0 len

(* Helper to write escape sequence *)
let write_escape t code =
  write_to_fd t.fd (Escape_seq.csi ^ code)
