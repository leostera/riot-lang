(** System utilities - file operations, directory management, etc. *)

(** Check if a file or directory exists *)
let file_exists = Sys.file_exists

(** Check if path is a directory *)
let is_directory = Sys.is_directory

(** Get current working directory *)
let getcwd = Sys.getcwd

(** Change current working directory *)
let chdir = Sys.chdir

(** Remove a file *)
let remove_file = Sys.remove

(** Make a file executable *)
let chmod path perm = Unix.chmod path perm

(** Create a directory if it doesn't exist, ignoring EEXIST errors *)
let mkdir_safe path perm =
  try Unix.mkdir path perm 
  with Unix.Unix_error (Unix.EEXIST, _, _) -> ()

(** Create a directory and all parent directories *)
let rec mkdirp path =
  if not (Sys.file_exists path) then begin
    mkdirp (Filename.dirname path);
    mkdir_safe path 0o755
  end

(** Copy a file from source to destination *)
let copy_file src dst =
  let ic = open_in_bin src in
  let oc = open_out_bin dst in
  (try
    while true do
      output_char oc (input_char ic)
    done
  with End_of_file -> ());
  close_in ic;
  close_out oc

(** List files in a directory with a filter function *)
let list_dir dir filter =
  let files = ref [] in
  if Sys.file_exists dir then begin
    let handle = Unix.opendir dir in
    (try
      while true do
        let file = Unix.readdir handle in
        if file <> "." && file <> ".." && filter file then
          files := file :: !files
      done
    with End_of_file -> ());
    Unix.closedir handle
  end;
  List.rev !files

(** List all files in a directory *)
let list_dir_all dir = list_dir dir (fun _ -> true)

(** Remove a directory recursively *)
let rec remove_dir dir =
  try
    let handle = Unix.opendir dir in
    (try
      while true do
        let file = Unix.readdir handle in
        if file <> "." && file <> ".." then begin
          let path = Filename.concat dir file in
          if Sys.is_directory path then
            remove_dir path
          else
            Sys.remove path
        end
      done
    with End_of_file -> ());
    Unix.closedir handle;
    Unix.rmdir dir
  with _ -> () (* Ignore cleanup errors *)

(** Run a shell command and return (success, output) *)
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
  | Unix.WEXITED 0 -> (true, output_str)
  | _ -> (false, output_str)

(** Read entire file as string *)
let read_file path =
  let ic = open_in path in
  let content = really_input_string ic (in_channel_length ic) in
  close_in ic;
  content

(** Write string to file *)
let write_file path content =
  let oc = open_out path in
  output_string oc content;
  close_out oc

(** Run a command and capture its output as a single string *)
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

(** Get number of CPU cores *)
let cpu_count () =
  match Sys.os_type with
  | "Unix" -> 
      (* Try different methods to get CPU count *)
      let try_command cmd =
        try
          let ic = Unix.open_process_in cmd in
          let result = input_line ic in
          ignore (Unix.close_process_in ic);
          int_of_string (String.trim result)
        with _ -> 0
      in
      let cores = 
        (* macOS *)
        let macos_cores = try_command "sysctl -n hw.ncpu" in
        if macos_cores > 0 then macos_cores
        else
          (* Linux *)
          let linux_cores = try_command "nproc" in
          if linux_cores > 0 then linux_cores
          else 4  (* Default fallback *)
      in
      cores
  | _ -> 4  (* Default for other systems *)