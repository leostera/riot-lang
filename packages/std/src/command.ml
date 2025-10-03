open Kernel.System

type status = int
type output = { stdout : string; stderr : string; status : status }
type error = SystemError of string

type state =
  | Pending
  | Running of {
      proc : OsProcess.t;
      stdout : Fs.File.t option;
      stderr : Fs.File.t option;
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
        OsProcess.
          { stdin = `Null; stdout = `Pipe; stderr = `Pipe }
      in

      (* Spawn the process *)
      match
        OsProcess.spawn ~program:t.cmd ~args:t.args ~env:t.env
          ?cwd:t.cwd ~stdio ()
      with
      | Error (`SpawnFailed msg) -> Error (SystemError msg)
      | Ok proc ->
          (* Get piped file descriptors *)
          let stdout_fd = OsProcess.stdout proc |> Option.unwrap |> Fs.File.from_fd in
          let stderr_fd = OsProcess.stderr proc |> Option.unwrap |> Fs.File.from_fd in

          (* Update state to Running *)
          t.state <- Running { proc; stdout = Some stdout_fd; stderr = Some stderr_fd};

          (* Read output BEFORE waiting - prevents deadlock if pipes fill up *)
          let stdout_str = Fs.File.read_to_end stdout_fd |> Result.unwrap in
          let stderr_str = Fs.File.read_to_end stderr_fd |> Result.unwrap in

          (* Close the File handles explicitly - we're done reading *)
          let _ = Fs.File.close stdout_fd in
          let _ = Fs.File.close stderr_fd in

          (* Now wait for process to exit *)
          let rec wait_for_exit () =
            match OsProcess.try_wait proc with
            | None ->
                Miniriot.yield ();
                wait_for_exit ()
            | Some status -> status
          in
          let exit_status = wait_for_exit () in

          (* Note: OsProcess.close would double-close the FDs we already closed above.
             Since we've already closed stdout/stderr via Fs.File.close, we don't call
             OsProcess.close here. The process itself has already exited. *)

          (* Convert status *)
          let status_code =
            match exit_status with
            | OsProcess.Running -> 0 (* Should not happen *)
            | OsProcess.Exited code -> code
            | OsProcess.Signaled n -> 128 + n
            | OsProcess.Stopped n -> 128 + n
          in

          let result =
            { status = status_code; stdout = stdout_str; stderr = stderr_str }
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
        OsProcess.
          { stdin = `Null; stdout = `Inherit; stderr = `Inherit }
      in

      (* Spawn the process *)
      match
        OsProcess.spawn ~program:t.cmd ~args:t.args ~env:t.env
          ?cwd:t.cwd ~stdio ()
      with
      | Error (`SpawnFailed msg) -> Error (SystemError msg)
      | Ok proc ->
          (* Wait for process to exit - async-friendly polling *)
          let rec wait_for_exit () =
            match OsProcess.try_wait proc with
            | None ->
                (* Still running - yield to other processes *)
                Miniriot.yield ();
                wait_for_exit ()
            | Some status -> status
          in
          let exit_status = wait_for_exit () in

          (* Close the process *)
          OsProcess.close proc;

          (* Convert status *)
          let status_code =
            match exit_status with
            | OsProcess.Running -> 0 (* Should not happen *)
            | OsProcess.Exited code -> code
            | OsProcess.Signaled n -> 128 + n
            | OsProcess.Stopped n -> 128 + n
          in

          (* Update state to Exited (with empty stdout/stderr since we didn't capture) *)
          t.state <- Exited { status = status_code; stdout = ""; stderr = "" };

          Ok status_code)
