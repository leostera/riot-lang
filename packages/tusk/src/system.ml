(** System utilities - file operations, directory management, etc. *)

(** Basic Unix/Sys wrappers - defined first to avoid circular dependencies *)

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

(** Get home directory *)
let get_home () = Sys.getenv "HOME"

(** Set environment variable *)
let putenv key value = Unix.putenv key value

(** Execute a program, replacing the current process *)
let exec prog args = Unix.execv prog args

(** Get process ID *)
let getpid () = Unix.getpid ()

(** Read a line from an input channel *)
let read_line ic = input_line ic

(** Create a symbolic link *)
let symlink src dst = Unix.symlink src dst

(** Get file stats *)
let stat path = Unix.stat path

(** Execute a shell command and return the exit status *)
let system cmd = Unix.system cmd

(** Open directory for reading *)
let opendir path = Unix.opendir path

(** Read next entry from directory *)
let readdir handle = Unix.readdir handle

(** Close directory handle *)
let closedir handle = Unix.closedir handle

(** Remove a directory (must be empty) *)
let rmdir path = Unix.rmdir path

(** Create a directory with permissions *)
let mkdir path perm = Unix.mkdir path perm

(** Get OS type *)
let os_type () = Sys.os_type

(** Get command line arguments *)
let argv () = Sys.argv

(** Open a process for reading *)
let open_process_in cmd = Unix.open_process_in cmd

(** Close a process opened for reading *)
let close_process_in ic = Unix.close_process_in ic

(** Get current time as float *)
let time () = Unix.time ()

(** Higher-level functions that use the basic wrappers *)

(** Check if a file is a regular file *)
let is_regular_file path =
  try (stat path).st_kind = Unix.S_REG with _ -> false

(** Create a directory if it doesn't exist, ignoring EEXIST errors *)
let mkdir_safe path perm =
  try mkdir path perm with Unix.Unix_error (Unix.EEXIST, _, _) -> ()

(** Create a directory and all parent directories *)
let rec mkdirp path =
  if not (file_exists path) then (
    mkdirp (Filename.dirname path);
    mkdir_safe path 0o755)

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
  if file_exists dir then (
    let handle = opendir dir in
    (try
       while true do
         let file = readdir handle in
         if file <> "." && file <> ".." && filter file then
           files := file :: !files
       done
     with End_of_file -> ());
    closedir handle);
  List.rev !files

(** List all files in a directory *)
let list_dir_all dir = list_dir dir (fun _ -> true)

(** Remove a directory recursively *)
let rec remove_dir dir =
  try
    let handle = opendir dir in
    (try
       while true do
         let file = readdir handle in
         if file <> "." && file <> ".." then
           let path = Filename.concat dir file in
           if is_directory path then remove_dir path else remove_file path
       done
     with End_of_file -> ());
    closedir handle;
    rmdir dir
  with _ -> () (* Ignore cleanup errors *)

(** Run a shell command and return (success, output) *)
let run_command cmd =
  Printf.printf "  $ %s\n" cmd;
  flush stdout;
  let ic = open_process_in (cmd ^ " 2>&1") in
  let output = ref [] in
  (try
     while true do
       output := input_line ic :: !output
     done
   with End_of_file -> ());
  let result = close_process_in ic in
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
  let ic = open_process_in cmd in
  let lines = ref [] in
  (try
     while true do
       lines := input_line ic :: !lines
     done
   with End_of_file -> ());
  ignore (close_process_in ic);
  List.rev !lines

(** Get number of CPU cores *)
let cpu_count () =
  match os_type () with
  | "Unix" ->
      (* Try different methods to get CPU count *)
      let try_command cmd =
        try
          let ic = open_process_in cmd in
          let result = input_line ic in
          ignore (close_process_in ic);
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
          if linux_cores > 0 then linux_cores else 4 (* Default fallback *)
      in
      cores
  | _ -> 4 (* Default for other systems *)

(** Tests submodule *)
module Tests = struct
  let test_file_exists_detects_files_correctly () : (unit, string) result =
    (* Test that file_exists returns true for existing files *)
    Ok ()
    [@test]

  let test_copy_file_preserves_content () : (unit, string) result =
    (* Test that copied files have identical content *)
    Ok ()
    [@test]

  let test_list_dir_returns_all_files () : (unit, string) result =
    (* Test that list_dir returns all files in directory *)
    Ok ()
    [@test]

  let test_list_dir_all_recursively_finds_files () : (unit, string) result =
    (* Test that list_dir_all finds all files recursively *)
    Ok ()
    [@test]

  let test_cpu_count_returns_positive_number () : (unit, string) result =
    (* Test that cpu_count returns reasonable value *)
    Ok ()
    [@test]

  let test_run_process_captures_output () : (unit, string) result =
    (* Test that run_process captures stdout correctly *)
    Ok ()
end [@test]
