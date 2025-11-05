open Std

type t = { rows : int; cols : int }

(* Get terminal size using the same Unix module that stdin.ml uses *)
external get_terminal_size : Unix.file_descr -> int * int = "caml_get_terminal_size"

let get () =
  try
    (* First try stdout *)
    let stdout_fd = Kernel.Fd.to_unix Kernel.IO.stdout in
    Log.debug "[SIZE.GET] stdout_fd = %d, isatty = %b" 
      (Obj.magic stdout_fd : int) (Unix.isatty stdout_fd);
    if Unix.isatty stdout_fd then begin
      let (cols, rows) = get_terminal_size stdout_fd in
      Log.debug "[SIZE.GET] get_terminal_size(stdout) returned cols=%d rows=%d" cols rows;
      Ok { rows; cols }
    end else begin
      (* If stdout is not a TTY, try opening /dev/tty directly *)
      Log.debug "[SIZE.GET] stdout not a TTY, trying /dev/tty";
      let tty_fd = Unix.openfile "/dev/tty" [Unix.O_RDWR] 0o666 in
      try
        let (cols, rows) = get_terminal_size tty_fd in
        Log.debug "[SIZE.GET] get_terminal_size(/dev/tty) returned cols=%d rows=%d" cols rows;
        Unix.close tty_fd;
        Ok { rows; cols }
      with e ->
        Unix.close tty_fd;
        raise e
    end
  with
  | Failure msg -> 
      Log.debug "[SIZE.GET] Failed with Failure: %s" msg;
      Error (`System_error msg)
  | Unix.Unix_error (err, _, _) -> 
      let msg = Unix.error_message err in
      Log.debug "[SIZE.GET] Failed with Unix_error: %s" msg;
      Error (`System_error msg)
  | e -> 
      Log.debug "[SIZE.GET] Failed with exception: %s" (Printexc.to_string e);
      Error (`System_error "Failed to get terminal size")

let to_string { rows; cols } = format "{ rows = %d; cols = %d }" rows cols
