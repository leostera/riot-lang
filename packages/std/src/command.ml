open Global
open Collections

type Runtime.Message.t +=
  | Reader_stdout_line of { reader: Runtime.Pid.t; line: string }
  | Reader_finished of { reader: Runtime.Pid.t; stream: 
        [
          `stdout
          | `stderr
        ]; result: (string, Fs.File.error) result }

module Stdio = struct
  type t =
    Null
    | Inherit
    | Pipe
    | File of Fs.File.t

  let null = fun () -> Null

  let inherit_ = fun () -> Inherit

  let pipe = fun () -> Pipe

  let from_file = fun file -> File file
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
  | Running of { proc: Kernel.Process.t; stdout: Fs.File.t option; stderr: Fs.File.t option }
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
  else if String.for_all ~fn:is_shell_safe_char value then
    value
  else
    "'" ^ String.concat "'\"'\"'" (String.split ~by:"'" value) ^ "'"

let to_string = fun t ->
  let command = String.concat " " (List.map (t.cmd :: t.args) ~fn:shell_quote) in
  let command =
    match t.env with
    | [] -> command
    | env -> String.concat
      " "
      (List.map env ~fn:(fun ((key, value)) -> key ^ "=" ^ shell_quote value))
    ^ " "
    ^ command
  in
  match t.cwd with
  | Some cwd -> "cd " ^ shell_quote cwd ^ " && " ^ command
  | None -> command

let spawn_reader = fun ?(line_mode = false) ~parent ~stream file ->
  spawn
    (fun () ->
      let reader = self () in
      let result =
        match stream, line_mode with
        | `stdout, true ->
            let buffer = IO.Buffer.create ~size:4096 in
            let rec loop () =
              match Fs.File.read_line file with
              | Ok line when String.equal line "" ->
                  Ok (IO.Buffer.contents buffer)
              | Ok line ->
                  IO.Buffer.add_string buffer line;
                  send parent (Reader_stdout_line { reader; line });
                  loop ()
              | Error _ as err ->
                  err
            in
            loop ()
        | _ ->
            Fs.File.read_to_end file
      in
      let _ = Fs.File.close file in
      send parent (Reader_finished { reader; stream; result });
      Ok ())

let wait_for_reader_output = fun ~on_stdout_line ~stdout_reader ~stderr_reader ->
  let stdout_result = ref None in
  let stderr_result = ref None in
  let rec loop () =
    if Option.is_some !stdout_result && Option.is_some !stderr_result then
      (Option.unwrap !stdout_result, Option.unwrap !stderr_result)
    else (
      receive
        ~selector:(
          function
          | Reader_stdout_line { reader; line } when Runtime.Pid.equal reader stdout_reader ->
              Option.for_each on_stdout_line ~fn:(fun on_stdout_line -> on_stdout_line line);
              `select ()
          | Reader_finished { reader; stream=`stdout; result } when Runtime.Pid.equal reader stdout_reader ->
              stdout_result := Some result;
              `select ()
          | Reader_finished { reader; stream=`stderr; result } when Runtime.Pid.equal reader stderr_reader ->
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
  | Ok output -> Ok output
  | Error err -> Error (SystemError ("Failed to read "
  ^ stream
  ^ " from command '"
  ^ cmd
  ^ "': "
  ^ Fs.File.error_to_string err))

let stdio_of_config = fun stdin stdout stderr ->
  let stdin_config =
    match stdin with
    | Stdio.Null -> Kernel.Process.Stdin.Null
    | Stdio.Inherit -> Kernel.Process.Stdin.Inherit
    | Stdio.Pipe -> Kernel.Process.Stdin.Pipe
    | Stdio.File file -> Kernel.Process.Stdin.File file
  in
  let stdout_config =
    match stdout with
    | Stdio.Null -> Kernel.Process.Stdout.Null
    | Stdio.Inherit -> Kernel.Process.Stdout.Inherit
    | Stdio.Pipe -> Kernel.Process.Stdout.Pipe
    | Stdio.File file -> Kernel.Process.Stdout.File file
  in
  let stderr_config =
    match stderr with
    | Stdio.Null -> Kernel.Process.Stderr.Null
    | Stdio.Inherit -> Kernel.Process.Stderr.Inherit
    | Stdio.Pipe -> Kernel.Process.Stderr.Pipe
    | Stdio.File file -> Kernel.Process.Stderr.File file
  in
  Kernel.Process.{ stdin = stdin_config; stdout = stdout_config; stderr = stderr_config }

let kernel_status_code = function
  | Kernel.Process.Running -> 0
  | Kernel.Process.Exited code -> code
  | Kernel.Process.Signaled n -> 128 + n
  | Kernel.Process.Stopped n -> 128 + n

let wait_for_exit = fun proc ->
  let source = Kernel.Process.to_source proc in
  let rec loop () =
    match Kernel.Process.try_wait proc with
    | Error err -> Error (SystemError (Kernel.Process.error_to_string err))
    | Ok None -> Runtime.syscall
      ~name:"Command.wait"
      ~interest:Kernel.Async.Interest.readable
      ~source
      loop
    | Ok (Some status) -> Ok status
  in
  loop ()

let cwd_path = fun cwd ->
  match cwd with
  | None -> Ok None
  | Some cwd -> Ok (Some (Kernel.Path.from_string cwd))

let output = fun ?on_stdout_line t ->
  match t.state with
  | Exited out ->
      Ok out
  | Running _ ->
      Error (SystemError "Command is already running")
  | Pending -> (
      (* Build stdio config to capture stdout and stderr *)
      let stdio = stdio_of_config Stdio.Null Stdio.Pipe Stdio.Pipe in
      match cwd_path t.cwd with
      | Error _ as err -> err
      | Ok current_dir -> (
          (* Spawn the process *)
          match Kernel.Process.spawn
            ~program:t.cmd
            ~args:(Array.from_list t.args)
            ~env:(Array.from_list t.env)
            ?current_dir
            ~stdio
            () with
          | Error err -> Error (SystemError (Kernel.Process.error_to_string err))
          | Ok proc ->
              (* Get piped file descriptors *)
              let stdout_fd = Kernel.Process.stdout proc |> Option.unwrap in
              let stderr_fd = Kernel.Process.stderr proc |> Option.unwrap in
              (* Update state to Running *)
              t.state <- Running { proc; stdout = Some stdout_fd; stderr = Some stderr_fd };
              let parent = self () in
              let stdout_reader =
                spawn_reader ~line_mode:(Option.is_some on_stdout_line) ~parent ~stream:`stdout stdout_fd
              in
              let stderr_reader = spawn_reader ~parent ~stream:`stderr stderr_fd in
              let stdout_result, stderr_result =
                wait_for_reader_output ~on_stdout_line ~stdout_reader ~stderr_reader
              in
              match unwrap_reader_result ~stream:"stdout" ~cmd:t.cmd stdout_result, unwrap_reader_result
                ~stream:"stderr"
                ~cmd:t.cmd
                stderr_result with
              | (Error _ as err), _ ->
                  err
              | _, (Error _ as err) ->
                  err
              | Ok stdout_str, Ok stderr_str ->
                  (* Now wait for process to exit *)
                  match wait_for_exit proc with
                  | Error _ as err -> err
                  | Ok exit_status ->
                      let status_code = kernel_status_code exit_status in
                      let result = { status = status_code; stdout = stdout_str; stderr = stderr_str } in
                      t.state <- Exited result;
                      Ok result
        )
    )

let status = fun t ->
  match t.state with
  | Exited out ->
      Ok out.status
  | Running _ ->
      Error (SystemError "Command is already running")
  | Pending -> (
      (* Build stdio config to inherit stdin, stdout and stderr (don't capture) *)
      let stdio = stdio_of_config Stdio.Inherit Stdio.Inherit Stdio.Inherit in
      match cwd_path t.cwd with
      | Error _ as err -> err
      | Ok current_dir -> (
          (* Spawn the process *)
          match Kernel.Process.spawn
            ~program:t.cmd
            ~args:(Array.from_list t.args)
            ~env:(Array.from_list t.env)
            ?current_dir
            ~stdio
            () with
          | Error err -> Error (SystemError (Kernel.Process.error_to_string err))
          | Ok proc ->
              match wait_for_exit proc with
              | Error _ as err -> err
              | Ok exit_status ->
                  let _ = Kernel.Process.close proc in
                  let status_code = kernel_status_code exit_status in
                  t.state <- Exited { status = status_code; stdout = ""; stderr = "" };
                  Ok status_code
        )
    )
