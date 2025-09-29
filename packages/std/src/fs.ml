(** Filesystem utilities *)

type error = SystemError of string

(** Helper to convert Kernel.IO errors to our error type *)
let kernel_error_to_string = function
  | `Noop -> "No operation"
  | `Eof -> "End of file"
  | `Timeout -> "Timeout"
  | `Process_down -> "Process down"
  | `Closed -> "Closed"
  | `Connection_closed -> "Connection closed"
  | `Exn exn -> Printexc.to_string exn
  | `No_info -> "No info"
  | `Would_block -> "Would block"
  | `Unix_error err -> Unix.error_message err
  | _ -> "Unknown error"

let convert_kernel_result = function
  | Ok v -> Ok v
  | Error e -> Error (SystemError (kernel_error_to_string e))

(** Basic filesystem operations - defined first as they're used by other
    functions *)

let is_directory path =
  let path_str = Path.to_string path in
  Kernel.IO.File.is_directory path_str |> convert_kernel_result

let is_regular_file path =
  let path_str = Path.to_string path in
  match Kernel.IO.File.stat path_str with
  | Ok stats -> Ok (stats.st_kind = Unix.S_REG)
  | Error _ -> Ok false

let remove_file path =
  let path_str = Path.to_string path in
  Kernel.IO.File.remove path_str |> convert_kernel_result

let rmdir path =
  let path_str = Path.to_string path in
  Kernel.IO.File.rmdir path_str |> convert_kernel_result

let opendir path =
  let path_str = Path.to_string path in
  Kernel.IO.File.opendir path_str |> convert_kernel_result

let readdir_handle handle =
  match Kernel.IO.File.readdir_handle handle with
  | Error `Eof -> Error (SystemError "End of directory")
  | result -> convert_kernel_result result

let closedir handle = Kernel.IO.File.closedir handle |> convert_kernel_result

let readdir path =
  match opendir path with
  | Error e -> Error e
  | Ok handle -> (
      let rec read_all acc =
        match readdir_handle handle with
        | Error _ -> List.rev acc (* End of directory or error *)
        | Ok entry ->
            if entry = "." || entry = ".." then read_all acc
            else read_all (entry :: acc)
      in
      let entries = read_all [] in
      match closedir handle with Error e -> Error e | Ok () -> Ok entries)

(** Directory reading iterator *)
module ReadDir = struct
  type t = { path : Path.t; handle : Unix.dir_handle; mutable closed : bool }
  type state = t
  type item = Path.t

  let create path =
    match opendir path with
    | Error e -> Error e
    | Ok handle -> Ok { path; handle; closed = false }

  let close t =
    if not t.closed then (
      t.closed <- true;
      try
        Kernel.IO.File.closedir t.handle |> ignore;
        Ok ()
      with e -> Error (SystemError (Printexc.to_string e)))
    else Ok ()

  let rec next t =
    if t.closed then None
    else
      try
        let entry =
          match Kernel.IO.File.readdir_handle t.handle with
          | Ok e -> e
          | Error _ -> raise End_of_file
        in
        if entry = "." || entry = ".." then next t (* Skip . and .. *)
        else
          match Path.of_string entry with
          | Ok p -> Some p
          | Error _ -> next t (* Skip invalid paths *)
      with End_of_file ->
        close t
        |> Result.expect
             ~msg:
               (Format.sprintf "Could not close ReadDir.t for %S"
                  (Path.to_string t.path));
        None

  (* MutIterator.Intf implementation *)
  let size _t = 0 (* Unknown size for directory iteration *)

  let clone t =
    (* Can't really clone a directory handle, so we create a new one *)
    match create t.path with
    | Ok new_t -> new_t
    | Error _ -> t (* Fall back to the original if we can't create a new one *)
end

(** Clean API implementations following the FIXME guidelines *)

let canonicalize path =
  let path_str = Path.to_string path in
  try
    let abs_path =
      match Kernel.IO.File.realpath path_str with
      | Ok p -> p
      | Error _ -> path_str
    in
    match Path.of_string abs_path with
    | Ok p -> Ok p
    | Error _ -> Error (SystemError "Invalid canonical path")
  with e -> Error (SystemError (Printexc.to_string e))

let copy ~src ~dst =
  let src_str = Path.to_string src in
  let dst_str = Path.to_string dst in
  try
    let ic = open_in_bin src_str in
    let oc = open_out_bin dst_str in
    let buf_size = 8192 in
    let buf = Bytes.create buf_size in
    let rec copy_loop () =
      let n = input ic buf 0 buf_size in
      if n > 0 then (
        output oc buf 0 n;
        copy_loop ())
    in
    copy_loop ();
    close_in ic;
    close_out oc;
    (* Copy permissions *)
    let stats =
      match Kernel.IO.File.stat src_str with
      | Ok s -> s
      | Error _ -> raise (Sys_error "Failed to stat source file")
    in
    (match Kernel.IO.File.chmod dst_str stats.st_perm with
    | Ok () -> ()
    | Error _ -> raise (Sys_error "Failed to chmod dest file"));
    Ok ()
  with e -> Error (SystemError (Printexc.to_string e))

let create_dir_all path =
  let rec create_parents path =
    match Path.parent path with
    | None -> Ok ()
    | Some parent ->
        if not (Path.exists parent) then
          match create_parents parent with
          | Error e -> Error e
          | Ok () -> (
              let parent_str = Path.to_string parent in
              try
                match Kernel.IO.File.mkdir parent_str 0o755 with
                | Ok () -> Ok ()
                | Error (`Unix_error Unix.EEXIST) -> Ok ()
                | Error e -> Error (SystemError (kernel_error_to_string e))
              with e -> Error (SystemError (Printexc.to_string e)))
        else Ok ()
  in
  match create_parents path with
  | Error e -> Error e
  | Ok () -> (
      let path_str = Path.to_string path in
      try
        match Kernel.IO.File.mkdir path_str 0o755 with
        | Ok () -> Ok ()
        | Error (`Unix_error Unix.EEXIST) -> Ok ()
        | Error e -> Error (SystemError (kernel_error_to_string e))
      with e -> Error (SystemError (Printexc.to_string e)))

let exists path = Ok (Path.exists path)

let hard_link ~src ~dst =
  let src_str = Path.to_string src in
  let dst_str = Path.to_string dst in
  try Kernel.IO.File.link src_str dst_str |> convert_kernel_result
  with e -> Error (SystemError (Printexc.to_string e))

let metadata path =
  let path_str = Path.to_string path in
  Kernel.IO.File.stat path_str |> convert_kernel_result

let read_to_string path =
  let path_str = Path.to_string path in
  try
    let ic = open_in_bin path_str in
    let len = in_channel_length ic in
    let buf = really_input_string ic len in
    close_in ic;
    Ok buf
  with e -> Error (SystemError (Printexc.to_string e))

let read_dir path =
  match ReadDir.create path with
  | Error e -> Error e
  | Ok state -> Ok (MutIterator.make (module ReadDir) state)

let remove_dir_all path =
  let rec remove_recursive path =
    match is_directory path with
    | Error e -> Error e
    | Ok false ->
        (* It's a file *)
        remove_file path
    | Ok true -> (
        (* It's a directory *)
        match readdir path with
        | Error e -> Error e
        | Ok entries ->
            let rec remove_entries = function
              | [] -> rmdir path
              | entry :: rest -> (
                  match Path.of_string entry with
                  | Error _ -> Error (SystemError ("Invalid path: " ^ entry))
                  | Ok entry_path -> (
                      let full_path = Path.join path entry_path in
                      match remove_recursive full_path with
                      | Error e -> Error e
                      | Ok () -> remove_entries rest))
            in
            remove_entries entries)
  in
  remove_recursive path

let rename ~src ~dst =
  let src_str = Path.to_string src in
  let dst_str = Path.to_string dst in
  try Kernel.IO.File.rename src_str dst_str |> convert_kernel_result
  with e -> Error (SystemError (Printexc.to_string e))

let set_permissions path perm =
  let path_str = Path.to_string path in
  Kernel.IO.File.chmod path_str perm |> convert_kernel_result

let write content path =
  let path_str = Path.to_string path in
  try
    let oc = open_out_bin path_str in
    output_string oc content;
    close_out oc;
    Ok ()
  with e -> Error (SystemError (Printexc.to_string e))

let read_link path =
  let path_str = Path.to_string path in
  try
    let target =
      match Kernel.IO.File.readlink path_str with
      | Ok t -> t
      | Error e -> raise (Sys_error (kernel_error_to_string e))
    in
    match Path.of_string target with
    | Ok p -> Ok p
    | Error _ -> Error (SystemError "Invalid link target")
  with e -> Error (SystemError (Printexc.to_string e))

let create_dir path =
  let path_str = Path.to_string path in
  match Kernel.IO.File.file_exists path_str with
  | Ok false | Error _ ->
      Kernel.IO.File.mkdir path_str 0o755 |> convert_kernel_result
  | Ok true -> Ok ()

let file_exists path =
  let path_str = Path.to_string path in
  Kernel.IO.File.file_exists path_str |> convert_kernel_result

let dir_exists path =
  let path_str = Path.to_string path in
  match Kernel.IO.File.file_exists path_str with
  | Ok true -> Kernel.IO.File.is_directory path_str |> convert_kernel_result
  | Ok false -> Ok false
  | Error e -> Error (SystemError (kernel_error_to_string e))

let stat path =
  let path_str = Path.to_string path in
  Kernel.IO.File.stat path_str |> convert_kernel_result

let chmod path perm =
  let path_str = Path.to_string path in
  Kernel.IO.File.chmod path_str perm |> convert_kernel_result

let symlink src dst =
  let src_str = Path.to_string src in
  let dst_str = Path.to_string dst in
  Kernel.IO.File.symlink src_str dst_str |> convert_kernel_result

let mkdir path perm =
  let path_str = Path.to_string path in
  Kernel.IO.File.mkdir path_str perm |> convert_kernel_result

let mkdir_safe path perm =
  let path_str = Path.to_string path in
  match Kernel.IO.File.mkdir path_str perm with
  | Ok () -> Ok ()
  | Error (`Unix_error Unix.EEXIST) -> Ok ()
  | Error e -> Error (SystemError (kernel_error_to_string e))

let rec mkdirp path =
  let path_str = Path.to_string path in
  if not (Kernel.IO.File.file_exists path_str = Ok true) then
    match Path.parent path with
    | Some parent ->
        let _ = mkdirp parent in
        mkdir_safe path 0o755
    | None -> mkdir_safe path 0o755
  else Ok ()

let rec remove_dir path =
  let path_str = Path.to_string path in
  match Kernel.IO.File.opendir path_str with
  | Error e -> Error (SystemError (kernel_error_to_string e))
  | Ok handle -> (
      let rec process_entries () =
        match Kernel.IO.File.readdir_handle handle with
        | Error `Eof -> Ok ()
        | Error e -> Error (SystemError (kernel_error_to_string e))
        | Ok file when file = "." || file = ".." -> process_entries ()
        | Ok file -> (
            let file_path =
              Path.join path
                (Path.of_string file |> Result.expect ~msg:"Invalid file path")
            in
            let file_path_str = Path.to_string file_path in
            match Kernel.IO.File.is_directory file_path_str with
            | Ok true -> (
                match remove_dir file_path with
                | Error e -> Error e
                | Ok () -> process_entries ())
            | Ok false | Error _ -> (
                match Kernel.IO.File.remove file_path_str with
                | Error e -> Error (SystemError (kernel_error_to_string e))
                | Ok () -> process_entries ()))
      in
      match process_entries () with
      | Error e ->
          let _ = Kernel.IO.File.closedir handle in
          Error e
      | Ok () -> (
          match Kernel.IO.File.closedir handle with
          | Error e -> Error (SystemError (kernel_error_to_string e))
          | Ok () -> Kernel.IO.File.rmdir path_str |> convert_kernel_result))

let copy_file src dst =
  let src_str = Path.to_string src in
  let dst_str = Path.to_string dst in
  try
    let ic = open_in_bin src_str in
    let oc = open_out_bin dst_str in
    (try
       while true do
         output_char oc (input_char ic)
       done
     with End_of_file -> ());
    close_in ic;
    close_out oc;
    Ok ()
  with e -> Error (SystemError (Printexc.to_string e))

let getcwd () =
  match Kernel.IO.File.getcwd () with
  | Ok cwd_str -> (
      match Path.of_string cwd_str with
      | Ok path -> Ok path
      | Error _ -> Error (SystemError "Invalid path"))
  | Error e -> Error (SystemError (kernel_error_to_string e))

let chdir path =
  Kernel.IO.File.chdir (Path.to_string path) |> convert_kernel_result

let temp_dir () =
  try
    match Path.of_string (Filename.get_temp_dir_name ()) with
    | Ok path -> Ok path
    | Error _ -> Error (SystemError "Invalid temp directory path")
  with e -> Error (SystemError (Printexc.to_string e))

let home_dir () =
  try
    match Kernel.Env.getenv "HOME" with
    | Some home -> (
        match Path.of_string home with
        | Ok path -> Ok path
        | Error _ -> Error (SystemError "Invalid home directory path"))
    | None -> Error (SystemError "HOME environment variable not set")
  with e -> Error (SystemError (Printexc.to_string e))

let file_size path =
  let path_str = Path.to_string path in
  match Kernel.IO.File.stat path_str with
  | Ok stats -> Ok stats.st_size
  | Error e -> Error (SystemError (kernel_error_to_string e))

let path_separator () = if Kernel.System.unix then "/" else "\\"

let current_executable () =
  try
    match Path.of_string Kernel.System.executable_name with
    | Ok path -> Ok path
    | Error _ -> Error (SystemError "Invalid executable path")
  with e -> Error (SystemError (Printexc.to_string e))

let is_absolute path = Path.is_absolute path
let is_relative path = Path.is_relative path
let join paths = List.fold_left Path.join (List.hd paths) (List.tl paths)

let read path =
  let path_str = Path.to_string path in
  try
    let ic = open_in_bin path_str in
    let len = in_channel_length ic in
    let content = really_input_string ic len in
    close_in ic;
    Ok content
  with e -> Error (SystemError (Printexc.to_string e))

let read_file = read
let write_file path content = write content path

(** Create a temporary directory, run a function with it, then clean it up *)
let with_tempdir ?(prefix = "tmp") fn =
  try
    let temp_base = Filename.get_temp_dir_name () in
    let temp_name = Filename.temp_dir ~temp_dir:temp_base prefix "" in
    match Path.of_string temp_name with
    | Error _ -> Error (SystemError "Failed to create temp directory")
    | Ok temp_path ->
        let result =
          try Ok (fn temp_path)
          with e -> Error (SystemError (Printexc.to_string e))
        in
        (* Clean up the temp directory *)
        let _ = remove_dir_all temp_path in
        result
  with e -> Error (SystemError (Printexc.to_string e))

(** List directory contents *)
let list_dir path =
  match readdir path with
  | Error e -> Error e
  | Ok entries ->
      let paths =
        List.filter_map
          (fun entry ->
            match Path.of_string entry with Ok p -> Some p | Error _ -> None)
          entries
      in
      Ok paths

(** Walk directory tree and call function on each path *)
let rec walk path fn =
  match is_directory path with
  | Error e -> Error e
  | Ok false ->
      fn path;
      Ok ()
  | Ok true -> (
      fn path;
      match readdir path with
      | Error e -> Error e
      | Ok entries ->
          let rec walk_entries = function
            | [] -> Ok ()
            | entry :: rest -> (
                let full_path =
                  Path.join path
                    (Path.of_string entry |> Result.expect ~msg:"Invalid path")
                in
                match walk full_path fn with
                | Error e -> Error e
                | Ok () -> walk_entries rest)
          in
          walk_entries (List.filter (fun e -> e <> "." && e <> "..") entries))
