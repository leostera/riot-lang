(** Filesystem utilities *)

open Global
open Iter
include Common
module Permissions = Permissions
module Metadata = Metadata
module ReadDir = ReadDir
module File = File
module Fd = Fd

(** Basic filesystem operations - defined first as they're used by other
    functions *)

let is_directory path =
  let path_str = Path.to_string path in
  Kernel.Fs.File.is_directory path_str |> convert_kernel_result

let rmdir path =
  let path_str = Path.to_string path in
  Kernel.Fs.File.rmdir path_str |> convert_kernel_result

let opendir path =
  let path_str = Path.to_string path in
  Kernel.Fs.File.opendir path_str |> convert_kernel_result

let readdir_handle handle =
  match Kernel.Fs.File.readdir_handle handle with
  | Error `Eof -> Error (SystemError "End of directory")
  | result -> convert_kernel_result result

let closedir handle = Kernel.Fs.File.closedir handle |> convert_kernel_result

(** Clean API implementations following the FIXME guidelines *)

let canonicalize path =
  let path_str = Path.to_string path in
  try
    let abs_path =
      match Kernel.Fs.File.realpath path_str with
      | Ok p -> p
      | Error _ -> path_str
    in
    match Path.of_string abs_path with
    | Ok p -> Ok p
    | Error _ -> Error (SystemError "Invalid canonical path")
  with e -> Error (SystemError (Exception.to_string e))

let copy ~src ~dst =
  let src_str = Path.to_string src in
  let dst_str = Path.to_string dst in
  match Kernel.Fs.File.copy_file src_str dst_str with
  | Ok () -> Ok ()
  | Error e -> Error (SystemError (kernel_error_to_string e))

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
                match Kernel.Fs.File.mkdir parent_str 0o755 with
                | Ok () -> Ok ()
                | Error (`IO_error Kernel.IO.File_exists) -> Ok ()
                | Error e -> Error (SystemError (kernel_error_to_string e))
              with e -> Error (SystemError (Exception.to_string e)))
        else Ok ()
  in
  match create_parents path with
  | Error e -> Error e
  | Ok () -> (
      let path_str = Path.to_string path in
      try
        match Kernel.Fs.File.mkdir path_str 0o755 with
        | Ok () -> Ok ()
        | Error (`IO_error Kernel.IO.File_exists) -> Ok ()
        | Error e -> Error (SystemError (kernel_error_to_string e))
      with e -> Error (SystemError (Exception.to_string e)))

let exists path = Ok (Path.exists path)

let hard_link ~src ~dst =
  let src_str = Path.to_string src in
  let dst_str = Path.to_string dst in
  try Kernel.Fs.File.link src_str dst_str |> convert_kernel_result
  with e -> Error (SystemError (Exception.to_string e))

let metadata path =
  let path_str = Path.to_string path in
  Kernel.Fs.File.stat path_str |> convert_kernel_result

let symlink_metadata path =
  let path_str = Path.to_string path in
  Kernel.Fs.File.lstat path_str |> convert_kernel_result

let read_to_string path =
  match File.open_read path with
  | Error e -> Error e
  | Ok file -> (
      match File.read_to_end file with
      | Error e ->
          let _ = File.close file in
          Error e
      | Ok content ->
          let _ = File.close file in
          Ok content)

let read_dir path =
  match ReadDir.create path with
  | Error e -> Error e
  | Ok state -> Ok (MutIterator.make (module ReadDir) state)

let remove_file path =
  let path_str = Path.to_string path in
  Kernel.Fs.File.remove path_str |> convert_kernel_result

let remove_dir_all path =
  let rec remove_recursive path =
    match is_directory path with
    | Error e -> Error e
    | Ok false ->
        (* It's a file *)
        remove_file path
    | Ok true -> (
        (* It's a directory *)
        match ReadDir.create path with
        | Error e -> Error e
        | Ok dir ->
            let rec remove_entries () =
              match ReadDir.next dir with
              | None -> rmdir path
              | Some entry_path -> (
                  let full_path = Path.join path entry_path in
                  match remove_recursive full_path with
                  | Error e -> Error e
                  | Ok () -> remove_entries ())
            in
            remove_entries ())
  in
  remove_recursive path

let rename ~src ~dst =
  let src_str = Path.to_string src in
  let dst_str = Path.to_string dst in
  try Kernel.Fs.File.rename src_str dst_str |> convert_kernel_result
  with e -> Error (SystemError (Exception.to_string e))

let set_permissions path perm =
  let path_str = Path.to_string path in
  Kernel.Fs.File.chmod path_str (Permissions.to_mode perm)
  |> convert_kernel_result

let write content path =
  let path_str = Path.to_string path in
  let open Kernel.Fs.File in
  match open_file path_str [ WriteOnly; Create; Truncate ] 0o644 with
  | Error e -> Error (SystemError (kernel_error_to_string e))
  | Ok fd -> (
      let buf = Bytes.of_string content in
      let len = Bytes.length buf in
      match write fd buf ~len with
      | Error e ->
          let _ = close_fd fd in
          Error (SystemError (kernel_error_to_string e))
      | Ok _ -> (
          match close_fd fd with
          | Ok () -> Ok ()
          | Error e -> Error (SystemError (kernel_error_to_string e))))

let read_link path =
  let path_str = Path.to_string path in
  try
    let target =
      match Kernel.Fs.File.readlink path_str with
      | Ok t -> t
      | Error e -> raise (Sys_error (kernel_error_to_string e))
    in
    match Path.of_string target with
    | Ok p -> Ok p
    | Error _ -> Error (SystemError "Invalid link target")
  with e -> Error (SystemError (Exception.to_string e))

let create_dir path =
  let path_str = Path.to_string path in
  match Kernel.Fs.File.file_exists path_str with
  | Ok false | Error _ ->
      Kernel.Fs.File.mkdir path_str 0o755 |> convert_kernel_result
  | Ok true -> Ok ()

let file_exists path =
  let path_str = Path.to_string path in
  Kernel.Fs.File.file_exists path_str |> convert_kernel_result

let dir_exists path =
  let path_str = Path.to_string path in
  match Kernel.Fs.File.file_exists path_str with
  | Ok true -> Kernel.Fs.File.is_directory path_str |> convert_kernel_result
  | Ok false -> Ok false
  | Error e -> Error (SystemError (kernel_error_to_string e))

let stat path =
  let path_str = Path.to_string path in
  Kernel.Fs.File.stat path_str |> convert_kernel_result

let chmod path perm =
  let path_str = Path.to_string path in
  Kernel.Fs.File.chmod path_str (Permissions.to_mode perm)
  |> convert_kernel_result

let symlink ~src ~dst =
  let src_str = Path.to_string src in
  let dst_str = Path.to_string dst in
  Kernel.Fs.File.symlink src_str dst_str |> convert_kernel_result

let mkdir path perm =
  let path_str = Path.to_string path in
  Kernel.Fs.File.mkdir path_str perm |> convert_kernel_result

let mkdir_safe path perm =
  let path_str = Path.to_string path in
  match Kernel.Fs.File.mkdir path_str perm with
  | Ok () -> Ok ()
  | Error (`IO_error Kernel.IO.File_exists) -> Ok ()
  | Error e -> Error (SystemError (kernel_error_to_string e))

let rec mkdirp path =
  let path_str = Path.to_string path in
  if not (Kernel.Fs.File.file_exists path_str = Ok true) then
    match Path.parent path with
    | Some parent ->
        let _ = mkdirp parent in
        mkdir_safe path 0o755
    | None -> mkdir_safe path 0o755
  else Ok ()

let rec remove_dir path =
  let path_str = Path.to_string path in
  match Kernel.Fs.File.opendir path_str with
  | Error e -> Error (SystemError (kernel_error_to_string e))
  | Ok handle -> (
      let rec process_entries () =
        match Kernel.Fs.File.readdir_handle handle with
        | Error `Eof -> Ok ()
        | Error e -> Error (SystemError (kernel_error_to_string e))
        | Ok file when file = "." || file = ".." -> process_entries ()
        | Ok file -> (
            let file_path =
              Path.join path
                (Path.of_string file |> Result.expect ~msg:"Invalid file path")
            in
            let file_path_str = Path.to_string file_path in
            match Kernel.Fs.File.is_directory file_path_str with
            | Ok true -> (
                match remove_dir file_path with
                | Error e -> Error e
                | Ok () -> process_entries ())
            | Ok false | Error _ -> (
                match Kernel.Fs.File.remove file_path_str with
                | Error e -> Error (SystemError (kernel_error_to_string e))
                | Ok () -> process_entries ()))
      in
      match process_entries () with
      | Error e ->
          let _ = Kernel.Fs.File.closedir handle in
          Error e
      | Ok () -> (
          match Kernel.Fs.File.closedir handle with
          | Error e -> Error (SystemError (kernel_error_to_string e))
          | Ok () -> Kernel.Fs.File.rmdir path_str |> convert_kernel_result))

let file_size path =
  let path_str = Path.to_string path in
  match Kernel.Fs.File.stat path_str with
  | Ok stats -> Ok (Kernel.Fs.File.Metadata.size stats)
  | Error e -> Error (SystemError (kernel_error_to_string e))

let path_separator () = if Kernel.System.unix then "/" else "\\"

let current_executable () =
  try
    match Path.of_string Kernel.System.executable_name with
    | Ok path -> Ok path
    | Error _ -> Error (SystemError "Invalid executable path")
  with e -> Error (SystemError (Exception.to_string e))

let is_absolute path = Path.is_absolute path
let is_relative path = Path.is_relative path
let join paths = List.fold_left Path.join (List.hd paths) (List.tl paths)

let read path =
  match File.open_read path with
  | Error e -> Error e
  | Ok file -> (
      match File.read_to_end file with
      | Error e ->
          let _ = File.close file in
          Error e
      | Ok content ->
          let _ = File.close file in
          Ok content)

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
          with e -> Error (SystemError (Exception.to_string e))
        in
        (* Clean up the temp directory *)
        let _ = remove_dir_all temp_path in
        result
  with e -> Error (SystemError (Exception.to_string e))

(** Walk directory tree and call function on each path *)
let rec walk path fn =
  match is_directory path with
  | Error e -> Error e
  | Ok false ->
      fn path;
      Ok ()
  | Ok true -> (
      fn path;
      match ReadDir.create path with
      | Error e -> Error e
      | Ok dir ->
          let rec walk_entries () =
            match ReadDir.next dir with
            | None -> Ok ()
            | Some entry_path -> (
                let full_path = Path.join path entry_path in
                match walk full_path fn with
                | Error e -> Error e
                | Ok () -> walk_entries ())
          in
          walk_entries ())

let is_file path =
  match metadata path with
  | Error e -> Error e
  | Ok m -> Ok (Metadata.is_file m)

let is_dir path =
  match metadata path with Error e -> Error e | Ok m -> Ok (Metadata.is_dir m)

let current_dir () =
  match Env.current_dir () with
  | Ok cwd -> Ok cwd
  | Error _ -> Error (SystemError "Failed to get current directory")
