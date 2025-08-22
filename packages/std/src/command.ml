(** Command - OS process spawning and management *)

type t = { pid : int; cmd : string }
type error = SpawnFailed of string | CommandNotFound of string

let spawn ~cmd ~args =
  try
    (* Use Unix.create_process to spawn a detached process *)
    (* For daemon processes, we need to detach from the parent's I/O *)
    let args_array = Array.of_list (cmd :: args) in
    (* Open /dev/null for stdin/stdout/stderr to detach the process *)
    let null_in = Unix.openfile "/dev/null" [ Unix.O_RDONLY ] 0o000 in
    let null_out = Unix.openfile "/dev/null" [ Unix.O_WRONLY ] 0o000 in
    let pid = Unix.create_process cmd args_array null_in null_out null_out in
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
    Unix.kill t.pid 0;
    true
  with Unix.Unix_error _ -> false

let kill t =
  try
    Unix.kill t.pid Sys.sigterm;
    Ok ()
  with Unix.Unix_error (_, _, _) as exn ->
    Error (SpawnFailed (Printexc.to_string exn))

let is_pid_running pid =
  try
    (* Send signal 0 to check if process exists *)
    Unix.kill pid 0;
    true
  with Unix.Unix_error _ -> false

let exec prog args = Unix.execv prog args
let getpid () = Unix.getpid ()
let system cmd = Unix.system cmd
let open_process_in cmd = Unix.open_process_in cmd
let close_process_in ic = Unix.close_process_in ic

let run_command cmd =
  Printf.printf "  $ %s\n" cmd;
  flush stdout;
  let ic = Unix.open_process_in (cmd ^ " 2>&1") in
  let output = ref [] in
  (try
     while true do
       output := input_line ic :: !output
     done
   with End_of_file -> ());
  let result = Unix.close_process_in ic in
  let output_str = String.concat "\n" (List.rev !output) in
  match result with
  | Unix.WEXITED 0 -> Ok output_str
  | _ -> Error (SpawnFailed output_str)

let run_process_lines cmd =
  let ic = Unix.open_process_in cmd in
  let lines = ref [] in
  (try
     while true do
       lines := input_line ic :: !lines
     done
   with End_of_file -> ());
  ignore (Unix.close_process_in ic);
  List.rev !lines

let executable_name = Sys.executable_name
let argv () = Sys.argv
