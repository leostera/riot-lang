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
  fd : Kernel.Fd.t;  (* Primary TTY fd - used for termios operations *)
  stdin : Kernel.Fd.t;  (* Input file descriptor *)
  stdout : Kernel.Fd.t;  (* Output file descriptor *)
  stderr : Kernel.Fd.t;  (* Error output file descriptor *)
  original_attrs : Kernel.Terminal.termios;
  mutable size : size;
  mutable mode : mode;
}

(* Helper to write to file descriptor using async-friendly Fs.File.write_all *)
let write_to_fd fd str =
  let file = Fs.File.from_fd fd in
  match Fs.File.write_all file str with
  | Ok () -> ()
  | Error _ -> () (* Silently ignore write errors for now *)

(* Helper to write escape sequence *)
let write_escape t code =
  write_to_fd t.stdout (Escape_seq.csi ^ code)
