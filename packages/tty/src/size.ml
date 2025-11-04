open Std

type t = { rows : int; cols : int }

(* Get terminal size using the same Unix module that stdin.ml uses *)
external get_terminal_size : Unix.file_descr -> int * int = "caml_get_terminal_size"

let get () =
  try
    (* First try stdout *)
    let stdout_fd = Kernel.Fd.to_unix Kernel.IO.stdout in
    if Unix.isatty stdout_fd then
      let (cols, rows) = get_terminal_size stdout_fd in
      Ok { rows; cols }
    else
      (* If stdout is not a TTY, try opening /dev/tty directly *)
      let tty_fd = Unix.openfile "/dev/tty" [Unix.O_RDWR] 0o666 in
      try
        let (cols, rows) = get_terminal_size tty_fd in
        Unix.close tty_fd;
        Ok { rows; cols }
      with e ->
        Unix.close tty_fd;
        raise e
  with
  | Failure msg -> Error (`System_error msg)
  | Unix.Unix_error (err, _, _) -> 
      Error (`System_error (Unix.error_message err))
  | _ -> Error (`System_error "Failed to get terminal size")

let to_string { rows; cols } = format "{ rows = %d; cols = %d }" rows cols
