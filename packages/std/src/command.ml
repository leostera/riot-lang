open Global
open Collections
open Kernel.System

module Stdio = struct
  type t = Null | Inherit | Pipe | File of Fs.Fd.t

  let null () = Null
  let inherit_ () = Inherit
  let pipe () = Pipe
  let from_fd fd = File fd
  let from_file file = File (Fs.File.into_fd file)
end

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
  { cmd; args; env; cwd; state = Pending }

let is_shell_safe_char = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9'
  | '_' | '-' | '.' | '/' | ':' | '+' | '=' | ',' | '@' | '%' -> true
  | _ -> false

let shell_quote value =
  if String.equal value "" then "''"
  else if String.for_all is_shell_safe_char value then value
  else
    "'"
    ^ String.concat "'\"'\"'" (String.split_on_char '\'' value)
    ^ "'"

let to_string t =
  let command =
    String.concat " " (List.map shell_quote (t.cmd :: t.args))
  in
  let command =
    match t.env with
    | [] -> command
    | env ->
        String.concat " "
          (List.map (fun (key, value) -> key ^ "=" ^ shell_quote value) env)
        ^ " " ^ command
  in
  match t.cwd with
  | Some cwd -> "cd " ^ shell_quote cwd ^ " && " ^ command
  | None -> command

let output t =
  match t.state with
  | Exited out -> Ok out
  | Running _ -> Error (SystemError "Command is already running")
  | Pending -> (
      (* Build stdio config to capture stdout and stderr *)
      let stdio = OsProcess.{ stdin = `Null; stdout = `Pipe; stderr = `Pipe } in

      (* Spawn the process *)
      match
        OsProcess.spawn ~program:t.cmd ~args:t.args ~env:t.env ?cwd:t.cwd ~stdio
          ()
      with
      | Error (`SpawnFailed msg) -> Error (SystemError msg)
      | Ok proc ->
          (* Get piped file descriptors *)
          let stdout_fd =
            OsProcess.stdout proc |> Option.unwrap |> Fs.File.from_fd
          in
          let stderr_fd =
            OsProcess.stderr proc |> Option.unwrap |> Fs.File.from_fd
          in

          (* Update state to Running *)
          t.state <-
            Running { proc; stdout = Some stdout_fd; stderr = Some stderr_fd };

          (* Read output BEFORE waiting - prevents deadlock if pipes fill up *)
          let stdout_str = 
            match Fs.File.read_to_end stdout_fd with
            | Ok s -> s
            | Error err -> 
                panic ("Failed to read stdout from command '" ^ t.cmd ^ "': " ^ IO.error_message err)
          in
          let stderr_str = 
            match Fs.File.read_to_end stderr_fd with
            | Ok s -> s
            | Error err ->
                panic ("Failed to read stderr from command '" ^ t.cmd ^ "': " ^ IO.error_message err)
          in

          (* Now wait for process to exit *)
          let rec wait_for_exit () =
            match OsProcess.try_wait proc with
            | None ->
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
      (* Build stdio config to inherit stdin, stdout and stderr (don't capture) *)
      let stdio =
        OsProcess.{ stdin = `Inherit; stdout = `Inherit; stderr = `Inherit }
      in

      (* Spawn the process *)
      match
        OsProcess.spawn ~program:t.cmd ~args:t.args ~env:t.env ?cwd:t.cwd ~stdio
          ()
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
