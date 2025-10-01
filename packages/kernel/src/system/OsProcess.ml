(** Async-friendly OS process spawning and management *)

type status = Running | Exited of int | Signaled of int | Stopped of int

type stdio_config = {
  stdin : [ `Null | `Pipe | `Inherit ];
  stdout : [ `Null | `Pipe | `Inherit ];
  stderr : [ `Null | `Pipe | `Inherit | `Redirect_to_stdout ];
}

type t = {
  pid : int;
  stdin_fd : Async.Fd.t option;
  stdout_fd : Async.Fd.t option;
  stderr_fd : Async.Fd.t option;
  mutable status : status;
}

let pid t = t.pid
let stdin t = t.stdin_fd
let stdout t = t.stdout_fd
let stderr t = t.stderr_fd

let spawn ~program ~args ?(env = []) ?cwd ~stdio () =
  try
    (* Prepare stdin *)
    let stdin_read, stdin_write, stdin_child, stdin_parent =
      match stdio.stdin with
      | `Null ->
          let null_fd = Unix.openfile "/dev/null" [ Unix.O_RDONLY ] 0 in
          (null_fd, None, null_fd, None)
      | `Inherit -> (Unix.stdin, None, Unix.stdin, None)
      | `Pipe ->
          let read_fd, write_fd = Unix.pipe () in
          (read_fd, Some write_fd, read_fd, Some write_fd)
    in

    (* Prepare stdout *)
    let stdout_write, stdout_read, stdout_child, stdout_parent =
      match stdio.stdout with
      | `Null ->
          let null_fd = Unix.openfile "/dev/null" [ Unix.O_WRONLY ] 0 in
          (null_fd, None, null_fd, None)
      | `Inherit -> (Unix.stdout, None, Unix.stdout, None)
      | `Pipe ->
          let read_fd, write_fd = Unix.pipe () in
          (write_fd, Some read_fd, write_fd, Some read_fd)
    in

    (* Prepare stderr *)
    let stderr_write, stderr_read, stderr_child, stderr_parent =
      match stdio.stderr with
      | `Null ->
          let null_fd = Unix.openfile "/dev/null" [ Unix.O_WRONLY ] 0 in
          (null_fd, None, null_fd, None)
      | `Inherit -> (Unix.stderr, None, Unix.stderr, None)
      | `Redirect_to_stdout -> (stdout_write, None, stdout_write, None)
      | `Pipe ->
          let read_fd, write_fd = Unix.pipe () in
          (write_fd, Some read_fd, write_fd, Some read_fd)
    in

    (* Build environment array *)
    let env_array =
      if env = [] then Unix.environment ()
      else
        let base_env = Array.to_list (Unix.environment ()) in
        let env_list =
          List.fold_left
            (fun acc (k, v) ->
              let kv = Printf.sprintf "%s=%s" k v in
              let without_key =
                List.filter
                  (fun s -> not (String.starts_with ~prefix:(k ^ "=") s))
                  acc
              in
              kv :: without_key)
            base_env env
        in
        Array.of_list env_list
    in

    (* Build argument array *)
    let argv = Array.of_list (program :: args) in

    (* Change directory if requested *)
    let original_cwd =
      match cwd with
      | Some dir ->
          let old = Unix.getcwd () in
          Unix.chdir dir;
          Some old
      | None -> None
    in

    (* Spawn the process *)
    let pid =
      Unix.create_process_env program argv env_array stdin_child stdout_child
        stderr_child
    in

    (* Restore original directory *)
    (match original_cwd with Some dir -> Unix.chdir dir | None -> ());

    (* Close child-side fds in parent *)
    if stdio.stdin = `Pipe then Unix.close stdin_child;
    if stdio.stdout = `Pipe then Unix.close stdout_child;
    if stdio.stderr = `Pipe && stdio.stderr <> `Redirect_to_stdout then
      Unix.close stderr_child;

    (* Set parent-side fds to non-blocking *)
    (match stdin_parent with Some fd -> Unix.set_nonblock fd | None -> ());
    (match stdout_parent with Some fd -> Unix.set_nonblock fd | None -> ());
    (match stderr_parent with Some fd -> Unix.set_nonblock fd | None -> ());

    Ok
      {
        pid;
        stdin_fd = stdin_parent;
        stdout_fd = stdout_parent;
        stderr_fd = stderr_parent;
        status = Running;
      }
  with
  | Unix.Unix_error (err, fn, arg) ->
      Error
        (`SpawnFailed
          (Printf.sprintf "%s: %s(%s)" (Unix.error_message err) fn arg))
  | exn -> Error (`SpawnFailed (Printexc.to_string exn))

let try_wait t =
  match t.status with
  | Running -> (
      try
        let pid, status = Unix.waitpid [ Unix.WNOHANG ] t.pid in
        if pid = 0 then None (* Still running *)
        else
          let new_status =
            match status with
            | Unix.WEXITED code ->
                t.status <- Exited code;
                Exited code
            | Unix.WSIGNALED signal ->
                t.status <- Signaled signal;
                Signaled signal
            | Unix.WSTOPPED signal ->
                t.status <- Stopped signal;
                Stopped signal
          in
          Some new_status
      with Unix.Unix_error (Unix.ECHILD, _, _) ->
        (* Process already reaped *)
        Some t.status)
  | _ -> Some t.status

let kill t ~signal = Unix.kill t.pid signal

let close t =
  (* Close all open file descriptors *)
  (match t.stdin_fd with Some fd -> Unix.close fd | None -> ());
  (match t.stdout_fd with Some fd -> Unix.close fd | None -> ());
  (match t.stderr_fd with Some fd -> Unix.close fd | None -> ())

let current_pid () = Unix.getpid ()
