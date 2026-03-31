open Global0
open Collections

(** Async-friendly OS process spawning and management *)
type status =
  Running
  | Exited of int
  | Signaled of int
  | Stopped of int

type stdio_config = {
  stdin:
    [
      `Null
      | `Pipe
      | `Inherit
      | `File of Fd.t
    ];
  stdout:
    [
      `Null
      | `Pipe
      | `Inherit
      | `File of Fd.t
    ];
  stderr:
    [
      `Null
      | `Pipe
      | `Inherit
      | `Redirect_to_stdout
      | `File of Fd.t
    ];
}

type t = {
  pid: int;
  stdin_fd: Fd.t option;
  stdout_fd: Fd.t option;
  stderr_fd: Fd.t option;
  mutable status: status;
}

let pid = fun t -> t.pid

let stdin = fun t -> t.stdin_fd

let stdout = fun t -> t.stdout_fd

let stderr = fun t -> t.stderr_fd

let spawn = fun ~program ~args ?(env = []) ?cwd ~stdio () ->
  try
    let stdin_read, stdin_write, stdin_child, stdin_parent =
      match stdio.stdin with
      | `Null ->
          let null_fd = Unix.openfile "/dev/null" [ Unix.O_RDONLY ] 0 in
          (null_fd, None, null_fd, None)
      | `Inherit ->
          (Unix.stdin, None, Unix.stdin, None)
      | `Pipe ->
          let read_fd, write_fd = Unix.pipe () in
          (read_fd, Some write_fd, read_fd, Some write_fd)
      | `File fd ->
          (Fd.to_unix fd, None, Fd.to_unix fd, None)
    in
    (* Prepare stdout *)
    let stdout_write, stdout_read, stdout_child, stdout_parent =
      match stdio.stdout with
      | `Null ->
          let null_fd = Unix.openfile "/dev/null" [ Unix.O_WRONLY ] 0 in
          (null_fd, None, null_fd, None)
      | `Inherit ->
          (Unix.stdout, None, Unix.stdout, None)
      | `Pipe ->
          let read_fd, write_fd = Unix.pipe () in
          (write_fd, Some read_fd, write_fd, Some read_fd)
      | `File fd ->
          (Fd.to_unix fd, None, Fd.to_unix fd, None)
    in
    (* Prepare stderr *)
    let stderr_write, stderr_read, stderr_child, stderr_parent =
      match stdio.stderr with
      | `Null ->
          let null_fd = Unix.openfile "/dev/null" [ Unix.O_WRONLY ] 0 in
          (null_fd, None, null_fd, None)
      | `Inherit ->
          (Unix.stderr, None, Unix.stderr, None)
      | `Pipe ->
          let read_fd, write_fd = Unix.pipe () in
          (write_fd, Some read_fd, write_fd, Some read_fd)
      | `Redirect_to_stdout ->
          (stdout_child, None, stdout_child, None)
      | `File fd ->
          (Fd.to_unix fd, None, Fd.to_unix fd, None)
    in
    (* Build environment array *)
    let env_array =
      if env = [] then
        unix__environment ()
      else
        let base_env = Array.to_list (unix__environment ()) in
        let env_list =
          List.fold_left
            (fun acc ((k, v)) ->
              let kv = k ^ "=" ^ v in
              let without_key =
                List.filter (fun s -> not (String.starts_with ~prefix:(((((k ^ "="))))) s)) acc
              in
              kv :: without_key)
            base_env
            env
        in
        Array.of_list env_list
    in
    (* Build argument array *)
    let argv = Array.of_list (program :: args) in
    (* Spawn the process *)
    let pid = Unix.create_process_env program argv env_array stdin_child stdout_child stderr_child in
    (* Close child-side fds in parent (but NOT File fds - caller owns those) *)
    (
      match stdio.stdin with
      | `Pipe
      | `Null -> Unix.close stdin_child
      | `Inherit
      | `File _ -> ()
    );
    (
      match stdio.stdout with
      | `Pipe
      | `Null -> Unix.close stdout_child
      | `Inherit
      | `File _ -> ()
    );
    (
      match stdio.stderr with
      | `Pipe
      | `Null -> Unix.close stderr_child
      | `Redirect_to_stdout -> ()
      | `Inherit
      | `File _ -> ()
    );
    Ok {
      pid;
      stdin_fd = Option.map Fd.of_unix stdin_parent;
      stdout_fd = Option.map Fd.of_unix stdout_parent;
      stderr_fd = Option.map Fd.of_unix stderr_parent;
      status = Running;

    }
  with
  | Unix.Unix_error (err, fn, arg) -> Error (`SpawnFailed (Unix.error_message err
  ^ ": "
  ^ fn
  ^ "("
  ^ arg
  ^ ")"))
  | exn -> Error (`SpawnFailed (Printexc.to_string exn))

let try_wait = fun t ->
  match t.status with
  | Running -> (
      try
        let pid, status = Unix.waitpid [ Unix.WNOHANG ] t.pid in
        if pid = 0 then
          None
          (* Still running *)
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
      with
      | Unix.Unix_error (Unix.ECHILD, _, _) ->
          (* Process already reaped *)
          Some t.status
    )
  | _ -> Some t.status

let kill = fun t ~signal ->
  Unix.kill t.pid signal

let close = fun t ->
  (* Close all open file descriptors *)
  (
    match t.stdout_fd with
    | Some fd -> Fd.close fd
    | None -> ()
  );
  (
    match t.stderr_fd with
    | Some fd -> Fd.close fd
    | None -> ()
  );
  (
    match t.stdin_fd with
    | Some fd -> Fd.close fd
    | None -> ()
  );
  ()

let current_pid = fun () -> Unix.getpid ()
