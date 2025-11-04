open Std

let () =
  Printf.printf "Testing terminal size detection...\n%!";
  
  (* Check if stdout is a TTY *)
  let stdout_fd = Kernel.Fd.to_unix Kernel.IO.stdout in
  let is_tty = Unix.isatty stdout_fd in
  Printf.printf "stdout is TTY: %b (fd=%d)\n%!" is_tty (Obj.magic stdout_fd : int);
  
  (* Try stderr instead *)
  let stderr_fd = Kernel.Fd.to_unix Kernel.IO.stderr in
  let is_tty_err = Unix.isatty stderr_fd in
  Printf.printf "stderr is TTY: %b (fd=%d)\n%!" is_tty_err (Obj.magic stderr_fd : int);
  
  match Tty.Terminal.size () with
  | Ok (width, height) ->
      Printf.printf "Terminal.size() succeeded: %d columns x %d rows\n%!" width height
  | Error (`System_error msg) ->
      Printf.printf "Terminal.size() failed: %s\n%!" msg
