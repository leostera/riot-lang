open Prelude

let ( let* ) = Result.and_then

type error =
  | File of Fs.File.error
  | InvalidStatus of { tag: int }
  | System of System_error.t

type status =
  | Running
  | Exited of int
  | Signaled of int
  | Stopped of int

module Stdin = struct
  type t =
    | Null
    | Pipe
    | Inherit
    | File of Fs.File.t
end

module Stdout = struct
  type t =
    | Null
    | Pipe
    | Inherit
    | File of Fs.File.t
end

module Stderr = struct
  type t =
    | Null
    | Pipe
    | Inherit
    | RedirectToStdout
    | File of Fs.File.t
end

type input_stdio = Stdin.t

type output_stdio = Stdout.t

type error_stdio = Stderr.t

type stdio_config = {
  stdin: input_stdio;
  stdout: output_stdio;
  stderr: error_stdio;
}

type t = {
  pid: int;
  stdin_pipe: Fs.File.t option;
  stdout_pipe: Fs.File.t option;
  stderr_pipe: Fs.File.t option;
  mutable status: status;
}

type raw_stdio = {
  stdin_mode: int;
  stdin_file: Fs.File.t option;
  stdout_mode: int;
  stdout_file: Fs.File.t option;
  stderr_mode: int;
  stderr_file: Fs.File.t option;
}

let stdio_null = 0

let stdio_pipe = 1

let stdio_inherit = 2

let stdio_file = 3

let stdio_redirect_to_stdout = 4

let status_exited = 0

let status_signaled = 1

let status_stopped = 2

module FFI = struct
  external spawn:
    string ->
    string array ->
    (string * string) array ->
    string option ->
    raw_stdio ->
    ((int * Fs.File.t option * Fs.File.t option * Fs.File.t option), int) Result.t
    = "kernel_new_process_spawn"

  external try_wait: int -> (((int * int) option), int) Result.t = "kernel_new_process_try_wait"

  external kill: int -> int -> (unit, int) Result.t = "kernel_new_process_kill"

  external current_pid: unit -> int = "kernel_new_process_current_pid"
end

let default_stdio = { stdin = Stdin.Inherit; stdout = Stdout.Inherit; stderr = Stderr.Inherit }

let encode_input_stdio = fun value ->
  match value with
  | Stdin.Null -> (stdio_null, None)
  | Stdin.Pipe -> (stdio_pipe, None)
  | Stdin.Inherit -> (stdio_inherit, None)
  | Stdin.File file -> (stdio_file, Some file)

let encode_output_stdio = fun value ->
  match value with
  | Stdout.Null -> (stdio_null, None)
  | Stdout.Pipe -> (stdio_pipe, None)
  | Stdout.Inherit -> (stdio_inherit, None)
  | Stdout.File file -> (stdio_file, Some file)

let encode_error_stdio = fun value ->
  match value with
  | Stderr.Null -> (stdio_null, None)
  | Stderr.Pipe -> (stdio_pipe, None)
  | Stderr.Inherit -> (stdio_inherit, None)
  | Stderr.RedirectToStdout -> (stdio_redirect_to_stdout, None)
  | Stderr.File file -> (stdio_file, Some file)

let raw_stdio_of_config = fun config ->
  let stdin_mode, stdin_file = encode_input_stdio config.stdin in
  let stdout_mode, stdout_file = encode_output_stdio config.stdout in
  let stderr_mode, stderr_file = encode_error_stdio config.stderr in
  {
    stdin_mode;
    stdin_file;
    stdout_mode;
    stdout_file;
    stderr_mode;
    stderr_file;
  }

let status_of_raw = fun tag code ->
  match tag with
  | 0 -> Result.Ok (Exited code)
  | 1 -> Result.Ok (Signaled code)
  | 2 -> Result.Ok (Stopped code)
  | _ -> Result.Error (InvalidStatus { tag })

let error_to_string = fun value ->
  match value with
  | File error -> Fs.File.error_to_string error
  | InvalidStatus { tag } -> String.concat "" [ "invalid process status tag: "; Int.to_string tag ]
  | System error -> System_error.to_string error

let pid = fun process -> process.pid

let stdin = fun process -> process.stdin_pipe

let stdout = fun process -> process.stdout_pipe

let stderr = fun process -> process.stderr_pipe

let spawn = fun ~program ~args ?env ?current_dir ~stdio () ->
  let env = Option.unwrap_or env ~default:[||] in
  let current_dir = Option.map Path.to_string current_dir in
  let raw_stdio = raw_stdio_of_config stdio in
  Result.map_error (fun code -> System (System_error.of_code code))
    (
      Result.map
        (fun (pid, stdin_pipe, stdout_pipe, stderr_pipe) ->
          {
            pid;
            stdin_pipe;
            stdout_pipe;
            stderr_pipe;
            status = Running;
          })
        (FFI.spawn program args env current_dir raw_stdio)
    )

let try_wait = fun process ->
  match process.status with
  | Exited _
  | Signaled _
  | Stopped _ -> Result.Ok (Some process.status)
  | Running ->
      let* status =
        Result.map_error (fun code -> System (System_error.of_code code)) (FFI.try_wait process.pid)
      in
      match status with
      | None -> Result.Ok None
      | Some (tag, code) ->
          let* status = status_of_raw tag code in
          process.status <- status;
          Result.Ok (Some status)

let to_source = fun process ->
  let module Source = struct
    type nonrec t = t

    let register = fun process selector token _interest ->
      Async.Adapter.Selector.register_process selector ~pid:process.pid ~token

    let reregister = fun process selector token _interest ->
      Async.Adapter.Selector.reregister_process selector ~pid:process.pid ~token

    let deregister = fun process selector ->
      Async.Adapter.Selector.deregister_process selector ~pid:process.pid
  end in
  Async.Source.make (module Source) process

let kill = fun process ~signal ->
  Result.map_error (fun code -> System (System_error.of_code code)) (FFI.kill process.pid signal)

let close = fun process ->
  let rec close_all first_error = function
    | [] -> (
        match first_error with
        | Some error -> Result.Error error
        | None -> Result.Ok ()
      )
    | file :: rest ->
        let next_error =
          match (first_error, Fs.File.close file) with
          | (Some error, _) -> Some error
          | (None, Result.Ok ()) -> None
          | (None, Result.Error (Fs.File.System System_error.BadFileDescriptor)) -> None
          | (None, Result.Error error) -> Some (File error)
        in
        close_all next_error rest
  in
  let files =
    let files =
      match process.stdin_pipe with
      | Some file -> [ file ]
      | None -> []
    in
    let files =
      match process.stdout_pipe with
      | Some file -> file :: files
      | None -> files
    in
    match process.stderr_pipe with
    | Some file -> file :: files
    | None -> files
  in
  close_all None files

let current_pid = FFI.current_pid
