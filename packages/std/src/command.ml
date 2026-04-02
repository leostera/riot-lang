open Global
open Collections
open Kernel.System

type Actors.Message.t +=
  | Reader_finished of { reader: Actors.Pid.t; stream: 
        [
          `stdout
          | `stderr
        ]; result: (string, IO.error) result }

module Stdio = struct
  type t =
    Null
    | Inherit
    | Pipe
    | File of Fs.Fd.t

  let null = fun () -> Null

  let inherit_ = fun () -> Inherit

  let pipe = fun () -> Pipe

  let from_fd = fun fd -> File fd

  let from_file = fun file -> File (Fs.File.into_fd file)
end

type status = int

type output = {
  stdout: string;
  stderr: string;
  status: status;
}

type error =
  SystemError of string

type state =
  | Pending
  | Running of { proc: OsProcess.t; stdout: Fs.File.t option; stderr: Fs.File.t option }
  | Exited of output

type t = {
  cmd: string;
  args: string list;
  env: (string * string) list;
  cwd: string option;
  mutable state: state;
}

(** Command - OS process spawning and management *)
let make = fun ?cwd ?(env = []) ?(args = []) cmd ->
  {
    cmd;
    args;
    env;
    cwd;
    state = Pending;
  }

let is_shell_safe_char = function
  | 'a' .. 'z'
  | 'A' .. 'Z'
  | '0' .. '9'
  | '_'
  | '-'
  | '.'
  | '/'
  | ':'
  | '+'
  | '='
  | ','
  | '@'
  | '%' -> true
  | _ -> false

let shell_quote = fun value ->
  if String.equal value "" then
    "''"
  else if String.for_all is_shell_safe_char value then
    value
  else
    "'" ^ String.concat "'\"'\"'" (String.split_on_char '\'' value) ^ "'"

let to_string = fun t ->
  let command = String.concat " " (List.map shell_quote (t.cmd :: t.args)) in
  let command =
    match t.env with
    | [] -> command
    | env -> String.concat " " (List.map (fun ((key, value)) -> key ^ "=" ^ shell_quote value) env)
    ^ " "
    ^ command
  in
  match t.cwd with
  | Some cwd -> "cd " ^ shell_quote cwd ^ " && " ^ command
  | None -> command

let spawn_reader = fun ~parent ~stream file ->
  spawn
    (fun () ->
      let reader = self () in
      let result = Fs.File.read_to_end file in
      send parent (Reader_finished { reader; stream; result });
      Ok ())

let wait_for_reader_output = fun ~stdout_reader ~stderr_reader ->
  let stdout_result = ref None in
  let stderr_result = ref None in
  let rec loop () =
    if Option.is_some !stdout_result && Option.is_some !stderr_result then
      (Option.unwrap !stdout_result, Option.unwrap !stderr_result)
    else (
      receive
        ~selector:(
          function
          | Reader_finished { reader; stream=`stdout; result } when Actors.Pid.equal reader stdout_reader ->
              stdout_result := Some result;
              `select ()
          | Reader_finished { reader; stream=`stderr; result } when Actors.Pid.equal reader stderr_reader ->
              stderr_result := Some result;
              `select ()
          | _ ->
              `skip
        )
        ();
      loop ()
    )
  in
  loop ()

let unwrap_reader_result = fun ~stream ~cmd ->
  function
  | Ok output -> output
  | Error err -> panic
    ("Failed to read " ^ stream ^ " from command '" ^ cmd ^ "': " ^ IO.error_message err)

let output = fun t ->
  match t.state with
  | Exited out ->
      Ok out
  | Running _ ->
      Error (SystemError "Command is already running")
  | Pending -> (
      (* Build stdio config to capture stdout and stderr *)
      let stdio = OsProcess.{ stdin = `Null; stdout = `Pipe; stderr = `Pipe } in
      (* Spawn the process *)
      match OsProcess.spawn ~program:t.cmd ~args:t.args ~env:t.env ?cwd:t.cwd ~stdio () with
      | Error (`SpawnFailed msg) -> Error (SystemError msg)
      | Ok proc ->
          (* Get piped file descriptors *)
          let stdout_fd = OsProcess.stdout proc |> Option.unwrap |> Fs.File.from_fd in
          let stderr_fd = OsProcess.stderr proc |> Option.unwrap |> Fs.File.from_fd in
          (* Update state to Running *)
          t.state <- Running { proc; stdout = Some stdout_fd; stderr = Some stderr_fd };
          let parent = self () in
          let stdout_reader = spawn_reader ~parent ~stream:`stdout stdout_fd in
          let stderr_reader = spawn_reader ~parent ~stream:`stderr stderr_fd in
          let stdout_result, stderr_result = wait_for_reader_output ~stdout_reader ~stderr_reader in
          let stdout_str = unwrap_reader_result ~stream:"stdout" ~cmd:t.cmd stdout_result in
          let stderr_str = unwrap_reader_result ~stream:"stderr" ~cmd:t.cmd stderr_result in
          (* Now wait for process to exit *)
          let rec wait_for_exit () =
            match OsProcess.try_wait proc with
            | None ->
                Actors.yield ();
                wait_for_exit ()
            | Some status -> status
          in
          let exit_status = wait_for_exit () in
          (* Close the process *)
          OsProcess.close proc;
          (* Convert status *)
          let status_code =
            match exit_status with
            | OsProcess.Running -> 0
            | OsProcess.Exited code -> code
            | OsProcess.Signaled n -> 128 + n
            | OsProcess.Stopped n -> 128 + n
          in
          let result = { status = status_code; stdout = stdout_str; stderr = stderr_str } in
          (* Update state to Exited *)
          t.state <- Exited result;
          Ok result
    )

let status = fun t ->
  match t.state with
  | Exited out ->
      Ok out.status
  | Running _ ->
      Error (SystemError "Command is already running")
  | Pending -> (
      (* Build stdio config to inherit stdin, stdout and stderr (don't capture) *)
      let stdio = OsProcess.{ stdin = `Inherit; stdout = `Inherit; stderr = `Inherit } in
      (* Spawn the process *)
      match OsProcess.spawn ~program:t.cmd ~args:t.args ~env:t.env ?cwd:t.cwd ~stdio () with
      | Error (`SpawnFailed msg) -> Error (SystemError msg)
      | Ok proc ->
          (* Wait for process to exit - async-friendly polling *)
          let rec wait_for_exit () =
            match OsProcess.try_wait proc with
            | None ->
                (* Still running - yield to other processes *)
                Actors.yield ();
                wait_for_exit ()
            | Some status -> status
          in
          let exit_status = wait_for_exit () in
          (* Close the process *)
          OsProcess.close proc;
          (* Convert status *)
          let status_code =
            match exit_status with
            | OsProcess.Running -> 0
            | OsProcess.Exited code -> code
            | OsProcess.Signaled n -> 128 + n
            | OsProcess.Stopped n -> 128 + n
          in
          (* Update state to Exited (with empty stdout/stderr since we didn't capture) *)
          t.state <- Exited { status = status_code; stdout = ""; stderr = "" };
          Ok status_code
    )
