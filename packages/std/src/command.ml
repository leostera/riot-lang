(** Command - OS process spawning and management *)

(** Process status types - for OS processes, not actors *)
type status = Exited of int | Signaled of int | Stopped of int

let of_unix_status = function
  | Unix.WEXITED code -> Exited code
  | Unix.WSIGNALED signal -> Signaled signal
  | Unix.WSTOPPED signal -> Stopped signal

type t = { pid : int; cmd : string }
type error = SpawnFailed of string | CommandNotFound of string

let show_error = function
  | SpawnFailed msg -> "Spawn failed: " ^ msg
  | CommandNotFound cmd -> "Command not found: " ^ cmd

type output = { status : int; stdout : string; stderr : string }
(** Output from a command execution *)

type cmd = {
  program : string;
  arguments : string list;
  environment : (string * string) list;
  stdin_cfg : [ `Pipe | `Null | `Inherit ];
  stdout_cfg : [ `Pipe | `Null | `Inherit ];
  stderr_cfg : [ `Pipe | `Null | `Inherit ];
}
(** Command builder type *)

(** Create a new command *)
let make program =
  {
    program;
    arguments = [];
    environment = [];
    stdin_cfg = `Inherit;
    stdout_cfg = `Pipe;
    stderr_cfg = `Pipe;
  }

(** Add a single argument *)
let arg arg cmd = { cmd with arguments = cmd.arguments @ [ arg ] }

(** Add multiple arguments *)
let args args cmd = { cmd with arguments = cmd.arguments @ args }

(** Set an environment variable *)
let env key value cmd =
  { cmd with environment = (key, value) :: cmd.environment }

(** Set multiple environment variables *)
let envs vars cmd = { cmd with environment = vars @ cmd.environment }

(** Configure stdin *)
let stdin cfg cmd = { cmd with stdin_cfg = cfg }

(** Configure stdout *)
let stdout cfg cmd = { cmd with stdout_cfg = cfg }

(** Configure stderr *)
let stderr cfg cmd = { cmd with stderr_cfg = cfg }

(** Helper to build the full command string with environment *)
let build_command_string cmd =
  let env_str =
    if cmd.environment = [] then ""
    else
      (cmd.environment
      |> List.map (fun (k, v) -> Printf.sprintf "%s=%s" k v)
      |> String.concat " ")
      ^ " "
  in
  let args_str = String.concat " " cmd.arguments in
  Printf.sprintf "%s%s %s" env_str cmd.program args_str

(** Execute command and capture output *)
let output cmd =
  (* Build the full command string *)
  let cmd_str = build_command_string cmd in
  Printf.printf "  $ %s\n%!" cmd_str;

  (* Use open_process_full to get stdin, stdout, and stderr *)
  let env_array =
    if cmd.environment = [] then Kernel.Osprocess.environment ()
    else
      let base_env = Kernel.Osprocess.environment () |> Array.to_list in
      let env_list =
        List.fold_left
          (fun acc (k, v) ->
            let kv = Printf.sprintf "%s=%s" k v in
            (* Replace existing or add new *)
            let without_key =
              List.filter
                (fun s -> not (String.starts_with ~prefix:(k ^ "=") s))
                acc
            in
            kv :: without_key)
          base_env cmd.environment
      in
      Array.of_list env_list
  in

  (* Execute the command *)
  let stdout_ic, stdin_oc, stderr_ic =
    Kernel.Osprocess.open_process_full
      (cmd.program ^ " " ^ String.concat " " cmd.arguments)
      env_array
  in

  (* Close stdin if not needed *)
  (match cmd.stdin_cfg with
  | `Null | `Inherit -> close_out stdin_oc
  | `Pipe -> ());

  (* Read stdout *)
  let stdout_lines = ref [] in
  (try
     while true do
       stdout_lines := input_line stdout_ic :: !stdout_lines
     done
   with End_of_file -> ());

  (* Read stderr *)
  let stderr_lines = ref [] in
  (try
     while true do
       stderr_lines := input_line stderr_ic :: !stderr_lines
     done
   with End_of_file -> ());

  (* Wait for process and get exit status *)
  let process_status =
    Kernel.Osprocess.close_process_full (stdout_ic, stdin_oc, stderr_ic)
  in

  let status_code =
    match process_status with
    | Unix.WEXITED code -> code
    | Unix.WSIGNALED n -> 128 + n
    | Unix.WSTOPPED n -> 128 + n
  in

  Ok
    {
      status = status_code;
      stdout = String.concat "\n" (List.rev !stdout_lines);
      stderr = String.concat "\n" (List.rev !stderr_lines);
    }

(** Execute command and return only the exit status *)
let status cmd =
  (* For status, we typically want output to go to the console *)
  let cmd_with_inherit = cmd |> stdout `Inherit |> stderr `Inherit in

  let cmd_str = build_command_string cmd_with_inherit in
  Printf.printf "  $ %s\n%!" cmd_str;

  (* Use system for simpler status-only execution when inheriting stdout/stderr *)
  match Kernel.Osprocess.system cmd_str with
  | Unix.WEXITED code -> Ok code
  | Unix.WSIGNALED n -> Ok (128 + n)
  | Unix.WSTOPPED n -> Ok (128 + n)

let spawn ~cmd ~args =
  try
    (* Use Unix.create_process to spawn a detached process *)
    (* For daemon processes, we need to detach from the parent's I/O *)
    let args_array = Array.of_list (cmd :: args) in
    (* Open /dev/null for stdin/stdout/stderr to detach the process *)
    let null_in = Unix.openfile "/dev/null" [ Unix.O_RDONLY ] 0o000 in
    let null_out = Unix.openfile "/dev/null" [ Unix.O_WRONLY ] 0o000 in
    let pid =
      Kernel.Osprocess.create_process cmd args_array null_in null_out null_out
    in
    Unix.close null_in;
    Unix.close null_out;
    Ok { pid; cmd }
  with
  | Unix.Unix_error (Unix.ENOENT, _, _) -> Error (CommandNotFound cmd)
  | exn -> Error (SpawnFailed (Printexc.to_string exn))

let pid t = t.pid

let is_running t =
  try
    (* Send signal 0 to check if process exists *)
    Kernel.Osprocess.kill t.pid 0;
    true
  with Unix.Unix_error _ -> false

let kill t =
  try
    Kernel.Osprocess.kill t.pid Kernel.Osprocess.sigterm;
    Ok ()
  with Unix.Unix_error (_, _, _) as exn ->
    Error (SpawnFailed (Printexc.to_string exn))

let is_pid_running pid =
  try
    (* Send signal 0 to check if process exists *)
    Kernel.Osprocess.kill pid 0;
    true
  with Unix.Unix_error _ -> false

let exec ?(args = []) prog () = Kernel.Osprocess.execv prog (Array.of_list args)
let getpid () = Kernel.Osprocess.getpid ()
let system cmd = Kernel.Osprocess.system cmd
let open_process_in cmd = Kernel.Osprocess.open_process_in cmd
let close_process_in ic = Kernel.Osprocess.close_process_in ic

let run_command ?env cmd_str =
  (* Legacy API - parse the command string and use the new API *)
  (* Split command string into program and arguments *)
  let parts = String.split_on_char ' ' cmd_str in
  match parts with
  | [] -> Error (CommandNotFound "Empty command")
  | prog :: args_list -> (
      let cmd_obj = make prog |> args args_list in
      let cmd_with_env =
        match env with
        | None -> cmd_obj
        | Some env_list -> envs env_list cmd_obj
      in
      (* For backward compatibility, combine stdout and stderr like 2>&1 *)
      match output cmd_with_env with
      | Ok out ->
          (* Combine stdout and stderr like the old version did with 2>&1 *)
          let combined =
            if out.stderr = "" then out.stdout
            else out.stdout ^ "\n" ^ out.stderr
          in
          if out.status = 0 then Ok combined else Error (SpawnFailed combined)
      | Error e -> Error e)

let run_process_lines cmd =
  let ic = Kernel.Osprocess.open_process_in cmd in
  let lines = ref [] in
  (try
     while true do
       lines := input_line ic :: !lines
     done
   with End_of_file -> ());
  ignore (Kernel.Osprocess.close_process_in ic);
  List.rev !lines

let executable_name = Kernel.System.executable_name
let argv () = Kernel.System.argv ()
