open Stdlib

let read_file = fun path ->
    let ic = open_in path in
    let len = in_channel_length ic in
    let content = really_input_string ic len in
    close_in ic;
    content

let write_file = fun path content ->
    let oc = open_out path in
    output_string oc content;
    close_out oc

let mkdir_p = fun dir ->
    let rec create_dirs path =
      if not (Sys.file_exists path) then
        (
          create_dirs (Filename.dirname path);
          try Unix.mkdir path 0o755 with
          | Unix.Unix_error (Unix.EEXIST, _, _) -> ()
        )
    in
    create_dirs dir

let copy_file = fun src dst ->
    let content = read_file src in
    write_file dst content

let copy_file_with_permissions = fun src dst ->
    (* Copy file content *)
    let content = read_file src in
    write_file dst content;
    (* Copy permissions *)
    let src_stat = Unix.stat src in
    Unix.chmod dst src_stat.st_perm

let run_command cmd : unit =
  let cmd = String.concat " " cmd in
  Printf.printf "  $ %s\n%!" cmd;
  let ret = Unix.system cmd in
  match ret with
  | Unix.WEXITED 0 -> ()
  | Unix.WEXITED n -> failwith (Printf.sprintf "Command failed with exit code %d" n)
  | Unix.WSIGNALED n -> failwith (Printf.sprintf "Command killed by signal %d" n)
  | Unix.WSTOPPED n -> failwith (Printf.sprintf "Command stopped by signal %d" n)

let run_command_with_output = fun cmd ->
    let cmd_str = String.concat " " cmd in
    Printf.printf "  $ %s\n%!" cmd_str;
    let stdout_ch, stdin_ch, stderr_ch = Unix.open_process_full cmd_str (Unix.environment ()) in
    close_out stdin_ch;
    let rec read_all ch acc =
      try
        let line = input_line ch in
        read_all ch (acc ^ line ^ "\n")
      with
      | End_of_file -> acc
    in
    let stdout = read_all stdout_ch "" in
    let stderr = read_all stderr_ch "" in
    let status = Unix.close_process_full (stdout_ch, stdin_ch, stderr_ch) in
    match status with
    | Unix.WEXITED 0 -> Ok stdout
    | _ -> Error stderr

let getcwd = Sys.getcwd

let chdir = Sys.chdir

let rm_rf = fun path -> run_command [ "rm"; "-rf"; path ]

let file_exists = Sys.file_exists
