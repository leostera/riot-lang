type status = int
type output = { stdout : string; stderr : string; status : status }
type error = SystemError of string

type state =
  | Pending
  | Running of {
      proc : Kernel.System.OsProcess.t;
      stdout_fd : Kernel.Async.Fd.t option;
      stderr_fd : Kernel.Async.Fd.t option;
    }
  | Exited of output

type t = {
  cmd : string;
  args : string list;
  env : (string * string) list;
  cwd : string option;
  mutable state : state;
}
(** Command - OS process spawning and management *)

let make ?cwd ?(env = []) ?(args = []) cmd =
  let cwd_str = Option.map Path.to_string cwd in
  { cmd; args; env; cwd = cwd_str; state = Pending }

let output t =
  match t.state with
  | Exited out -> Ok out
  | Running _ -> Error (SystemError "Command is already running")
  | Pending -> (
      (* Build stdio config to capture stdout and stderr *)
      let stdio =
        Kernel.System.OsProcess.
          { stdin = `Null; stdout = `Pipe; stderr = `Pipe }
      in

      (* Spawn the process *)
      match
        Kernel.System.OsProcess.spawn ~program:t.cmd ~args:t.args ~env:t.env
          ?cwd:t.cwd ~stdio ()
      with
      | Error (`SpawnFailed msg) -> Error (SystemError msg)
      | Ok proc ->
          (* Get piped file descriptors *)
          let stdout_fd = Kernel.System.OsProcess.stdout proc in
          let stderr_fd = Kernel.System.OsProcess.stderr proc in

          (* Update state to Running *)
          t.state <- Running { proc; stdout_fd; stderr_fd };

          (* Read from pipes while waiting for process to exit *)
          (* This prevents deadlock if pipes fill up *)
          let stdout_buf = Buffer.create 4096 in
          let stderr_buf = Buffer.create 4096 in
          let read_buffer = Bytes.create 4096 in

          let try_read_from_fd fd buf =
            match Kernel.Fs.File.read fd read_buffer ~pos:0 ~len:4096 with
            | Ok n ->
                if n > 0 then Buffer.add_subbytes buf read_buffer 0 n;
                n > 0 (* return true if we read data *)
            | Error (`Unix_error e) when Kernel.IO.error_of_unix e = Kernel.IO.Resource_unavailable_try_again -> false
            | Error (`Unix_error e) when Kernel.IO.error_of_unix e = Kernel.IO.Operation_would_block -> false
            | Error (`Unix_error e) when Kernel.IO.error_of_unix e = Kernel.IO.Broken_pipe -> false (* Broken pipe - process ended *)
            | Error _ -> false (* EOF or other errors *)
          in

          (* Wait for process to exit while draining pipes *)
          let rec wait_and_drain () =
            (* Try to read from pipes *)
            let stdout_read =
              match stdout_fd with
              | None -> false
              | Some fd -> try_read_from_fd fd stdout_buf
            in
            let stderr_read =
              match stderr_fd with
              | None -> false
              | Some fd -> try_read_from_fd fd stderr_buf
            in

            (* Check if process has exited *)
            match Kernel.System.OsProcess.try_wait proc with
            | Some status ->
                (* Process exited - drain any remaining data from pipes *)
                let rec drain_pipe_final fd buf =
                  if try_read_from_fd fd buf then drain_pipe_final fd buf
                in
                (match stdout_fd with
                | Some fd -> drain_pipe_final fd stdout_buf
                | None -> ());
                (match stderr_fd with
                | Some fd -> drain_pipe_final fd stderr_buf
                | None -> ());
                status
            | None ->
                (* Still running - yield if we didn't read anything *)
                if not (stdout_read || stderr_read) then Miniriot.yield ();
                wait_and_drain ()
          in

          let exit_status = wait_and_drain () in

          let stdout_data = Buffer.contents stdout_buf in
          let stderr_data = Buffer.contents stderr_buf in

          (* Close the process *)
          Kernel.System.OsProcess.close proc;

          (* Convert status *)
          let status_code =
            match exit_status with
            | Kernel.System.OsProcess.Running -> 0 (* Should not happen *)
            | Kernel.System.OsProcess.Exited code -> code
            | Kernel.System.OsProcess.Signaled n -> 128 + n
            | Kernel.System.OsProcess.Stopped n -> 128 + n
          in

          let result =
            { status = status_code; stdout = stdout_data; stderr = stderr_data }
          in

          (* Update state to Exited *)
          t.state <- Exited result;

          Ok result)

let status t =
  match t.state with
  | Exited out -> Ok out.status
  | Running _ -> Error (SystemError "Command is already running")
  | Pending -> (
      (* Build stdio config to inherit stdout and stderr (don't capture) *)
      let stdio =
        Kernel.System.OsProcess.
          { stdin = `Null; stdout = `Inherit; stderr = `Inherit }
      in

      (* Spawn the process *)
      match
        Kernel.System.OsProcess.spawn ~program:t.cmd ~args:t.args ~env:t.env
          ?cwd:t.cwd ~stdio ()
      with
      | Error (`SpawnFailed msg) -> Error (SystemError msg)
      | Ok proc ->
          (* Wait for process to exit - async-friendly polling *)
          let rec wait_for_exit () =
            match Kernel.System.OsProcess.try_wait proc with
            | None ->
                (* Still running - yield to other processes *)
                Miniriot.yield ();
                wait_for_exit ()
            | Some status -> status
          in
          let exit_status = wait_for_exit () in

          (* Close the process *)
          Kernel.System.OsProcess.close proc;

          (* Convert status *)
          let status_code =
            match exit_status with
            | Kernel.System.OsProcess.Running -> 0 (* Should not happen *)
            | Kernel.System.OsProcess.Exited code -> code
            | Kernel.System.OsProcess.Signaled n -> 128 + n
            | Kernel.System.OsProcess.Stopped n -> 128 + n
          in

          (* Update state to Exited (with empty stdout/stderr since we didn't capture) *)
          t.state <- Exited { status = status_code; stdout = ""; stderr = "" };

          Ok status_code)
