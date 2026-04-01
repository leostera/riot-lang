(** Filesystem utilities *)
open Global
open Iter
open IO
open Collections

include Common

module Event = Event
module Permissions = Permissions
module Metadata = Metadata
module ReadDir = ReadDir
module File = File
module Fd = Fd
module FileWatcher = File_watcher
(** Basic filesystem operations - defined first as they're used by other
    functions *)
let is_directory = fun path ->
  let path_str = Path.to_string path in
  Kernel.Fs.File.is_directory path_str |> convert_kernel_result

let rmdir = fun path ->
  let path_str = Path.to_string path in
  Kernel.Fs.File.rmdir path_str |> convert_kernel_result
(** Clean API implementations following the FIXME guidelines *)
let canonicalize = fun path ->
  let path_str = Path.to_string path in
  try
    let abs_path =
      match Kernel.Fs.File.realpath path_str with
      | Ok p -> p
      | Error _ -> path_str
    in
    match Path.of_string abs_path with
    | Ok p -> Ok p
    | Error _ -> Error (IO.Unknown_error "Invalid canonical path")
  with
  | e -> Error (IO.Unknown_error (Exception.to_string e))

let copy = fun ~src ~dst ->
  let src_str = Path.to_string src in
  let dst_str = Path.to_string dst in
  match Kernel.Fs.File.copy_file src_str dst_str with
  | Ok () -> Ok ()
  | Error e -> Error e

let create_dir_all = fun path ->
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
                | Error Kernel.IO.File_exists -> Ok ()
                | Error e -> Error e
              with
              | e -> Error (IO.Unknown_error (Exception.to_string e))
            )
        else
          Ok ()
  in
  match create_parents path with
  | Error e -> Error e
  | Ok () -> (
      let path_str = Path.to_string path in
      try
        match Kernel.Fs.File.mkdir path_str 0o755 with
        | Ok () -> Ok ()
        | Error Kernel.IO.File_exists -> Ok ()
        | Error e -> Error e
      with
      | e -> Error (IO.Unknown_error (Exception.to_string e))
    )

let exists = fun path -> Ok (Path.exists path)

let hard_link = fun ~src ~dst ->
  let src_str = Path.to_string src in
  let dst_str = Path.to_string dst in
  try Kernel.Fs.File.link src_str dst_str |> convert_kernel_result with
  | e -> Error (IO.Unknown_error (Exception.to_string e))

let metadata = fun path ->
  let path_str = Path.to_string path in
  Kernel.Fs.File.stat path_str |> convert_kernel_result

let symlink_metadata = fun path ->
  let path_str = Path.to_string path in
  Kernel.Fs.File.lstat path_str |> convert_kernel_result

let read_to_string = fun path ->
  match File.open_read path with
  | Error e -> Error e
  | Ok file -> (
      match File.read_to_end file with
      | Error e ->
          let _ = File.close file in
          Error e
      | Ok content ->
          let _ = File.close file in
          Ok content
    )

let read_dir = fun path ->
  match ReadDir.create path with
  | Error e -> Error e
  | Ok state -> Ok (MutIterator.make (module ReadDir) state)

let remove_file = fun path ->
  let path_str = Path.to_string path in
  Kernel.Fs.File.remove path_str |> convert_kernel_result

let remove_dir_all = fun path ->
  let rec remove_recursive path =
    match is_directory path with
    | Error e ->
        Error e
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
                  | Ok () -> remove_entries ()
                )
            in
            remove_entries ()
      )
  in
  remove_recursive path

let rename = fun ~src ~dst ->
  let src_str = Path.to_string src in
  let dst_str = Path.to_string dst in
  try Kernel.Fs.File.rename src_str dst_str |> convert_kernel_result with
  | e -> Error (IO.Unknown_error (Exception.to_string e))

let set_permissions = fun path perm ->
  let path_str = Path.to_string path in
  Kernel.Fs.File.chmod path_str (Permissions.to_mode perm) |> convert_kernel_result

let write = fun content path ->
  let path_str = Path.to_string path in
  let fd = Kernel.Fd.open_file
    path_str
    [ Kernel.Fd.OpenFlags.WriteOnly; Kernel.Fd.OpenFlags.Create; Kernel.Fd.OpenFlags.Truncate; ]
    0o644 in
  let buf = Bytes.of_string content in
  let len = Bytes.length buf in
  match Kernel.Fs.File.write fd buf ~len with
  | Error e ->
      let _ = Kernel.Fs.File.close_fd fd in
      Error e
  | Ok _ -> (
      match Kernel.Fs.File.close_fd fd with
      | Ok () -> Ok ()
      | Error e -> Error e
    )

let read_link = fun path ->
  let path_str = Path.to_string path in
  try
    let target =
      match Kernel.Fs.File.readlink path_str with
      | Ok t -> t
      | Error e -> raise (Sys_error (IO.error_message e))
    in
    match Path.of_string target with
    | Ok p -> Ok p
    | Error _ -> Error (IO.Unknown_error "Invalid link target")
  with
  | e -> Error (IO.Unknown_error (Exception.to_string e))

let create_dir = fun path ->
  let path_str = Path.to_string path in
  match Kernel.Fs.File.file_exists path_str with
  | Ok false
  | Error _ -> Kernel.Fs.File.mkdir path_str 0o755 |> convert_kernel_result
  | Ok true -> Ok ()

let file_exists = fun path ->
  let path_str = Path.to_string path in
  Kernel.Fs.File.file_exists path_str |> convert_kernel_result

let dir_exists = fun path ->
  let path_str = Path.to_string path in
  match Kernel.Fs.File.file_exists path_str with
  | Ok true -> Kernel.Fs.File.is_directory path_str |> convert_kernel_result
  | Ok false -> Ok false
  | Error e -> Error e

let stat = fun path ->
  let path_str = Path.to_string path in
  Kernel.Fs.File.stat path_str |> convert_kernel_result

let chmod = fun path perm ->
  let path_str = Path.to_string path in
  Kernel.Fs.File.chmod path_str (Permissions.to_mode perm) |> convert_kernel_result

let symlink = fun ~src ~dst ->
  let src_str = Path.to_string src in
  let dst_str = Path.to_string dst in
  Kernel.Fs.File.symlink src_str dst_str |> convert_kernel_result

let mkdir = fun path perm ->
  let path_str = Path.to_string path in
  Kernel.Fs.File.mkdir path_str perm |> convert_kernel_result

let mkdir_safe = fun path perm ->
  let path_str = Path.to_string path in
  match Kernel.Fs.File.mkdir path_str perm with
  | Ok () -> Ok ()
  | Error Kernel.IO.File_exists -> Ok ()
  | Error e -> Error e

let rec mkdirp = fun path ->
  let path_str = Path.to_string path in
  if not (Kernel.Fs.File.file_exists path_str = Ok true) then
    match Path.parent path with
    | Some parent ->
        let _ = mkdirp parent in
        mkdir_safe path 0o755
    | None -> mkdir_safe path 0o755
  else
    Ok ()

let rec remove_dir = fun path ->
  let path_str = Path.to_string path in
  match Kernel.Fs.ReadDir.open_ path_str with
  | Error e -> Error e
  | Ok handle -> (
      let rec process_entries () =
        match Kernel.Fs.ReadDir.read handle with
        | Error IO.End_of_file ->
            Ok ()
        | Error e ->
            Error e
        | Ok file when file = "." || file = ".." ->
            process_entries ()
        | Ok file -> (
            let file_path = Path.join path
              (Path.of_string file |> Result.expect ~msg:"Invalid file path")
            in
            let file_path_str = Path.to_string file_path in
            match Kernel.Fs.File.is_directory file_path_str with
            | Ok true -> (
                match remove_dir file_path with
                | Error e -> Error e
                | Ok () -> process_entries ()
              )
            | Ok false
            | Error _ -> (
                match Kernel.Fs.File.remove file_path_str with
                | Error e -> Error e
                | Ok () -> process_entries ()
              )
          )
      in
      match process_entries () with
      | Error e ->
          let _ = Kernel.Fs.ReadDir.close handle in
          Error e
      | Ok () -> (
          match Kernel.Fs.ReadDir.close handle with
          | Error e -> Error e
          | Ok () -> Kernel.Fs.File.rmdir path_str |> convert_kernel_result
        )
    )

let file_size = fun path ->
  let path_str = Path.to_string path in
  match Kernel.Fs.File.stat path_str with
  | Ok stats -> Ok (Kernel.Fs.File.Metadata.size stats)
  | Error e -> Error e

let path_separator = fun () ->
  if Kernel.System.unix then
    "/"
  else
    "\\"

let current_executable = fun () ->
  try
    match Path.of_string Kernel.System.executable_name with
    | Ok path -> Ok path
    | Error _ -> Error (IO.Unknown_error "Invalid executable path")
  with
  | e -> Error (IO.Unknown_error (Exception.to_string e))

let is_absolute = fun path -> Path.is_absolute path

let is_relative = fun path -> Path.is_relative path

let join = fun paths ->
  List.fold_left Path.join (List.hd paths) (List.tl paths)

let read = fun path ->
  match File.open_read path with
  | Error e -> Error e
  | Ok file -> (
      match File.read_to_end file with
      | Error e ->
          let _ = File.close file in
          Error e
      | Ok content ->
          let _ = File.close file in
          Ok content
    )

let read_file = read

let write_file = fun path content -> write content path
(** Get system temp directory *)
let get_temp_dir = fun () ->
  (* Try TMPDIR, TEMP, TMP environment variables, fallback to /tmp *)
  match Env.var String ~name:"TMPDIR" with
  | Some dir when dir != "" -> dir
  | _ -> (
      match Env.var String ~name:"TEMP" with
      | Some dir when dir != "" -> dir
      | _ -> (
          match Env.var String ~name:"TMP" with
          | Some dir when dir != "" -> dir
          | _ -> "/tmp"
        )
    )
(** Create a unique temporary directory name *)
let make_temp_dir_name = fun temp_base prefix ->
  let pid = Kernel.System.OsProcess.current_pid () in
  let random_suffix = Kernel.Random.bits () land 0xff_ffff in
  (* Convert to 6-digit hex string with leading zeros *)
  let hex_suffix =
    let hex_chars = "0123456789abcdef" in
    let s = Bytes.create 6 in
    let n = ref random_suffix in
    for i = 5 downto 0 do
      Bytes.set s i hex_chars.[!n land 0xf];
      n := !n lsr 4
    done;
    Bytes.to_string s
  in
  let dir_name = prefix ^ string_of_int pid ^ "_" ^ hex_suffix in
  temp_base ^ "/" ^ dir_name
(** Create a temporary directory, run a function with it, then clean it up *)
let with_tempdir = fun ?(prefix = "tmp") fn ->
  try
    let temp_base = get_temp_dir () in
    let temp_name = make_temp_dir_name temp_base prefix in
    match Path.of_string temp_name with
    | Error _ -> Error (IO.Unknown_error "Failed to create temp directory")
    | Ok temp_path -> (
        match create_dir temp_path with
        | Error e -> Error e
        | Ok () ->
            let result =
              try Ok (fn temp_path) with
              | e -> Error (IO.Unknown_error (Exception.to_string e))
            in
            (* Clean up the temp directory *)
            let _ = remove_dir_all temp_path in
            result
      )
  with
  | e -> Error (IO.Unknown_error (Exception.to_string e))
(** Walk directory tree and call function on each path *)
let rec walk = fun path fn ->
  match is_directory path with
  | Error e ->
      Error e
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
                | Ok () -> walk_entries ()
              )
          in
          walk_entries ()
    )

let is_file = fun path ->
  match metadata path with
  | Error e -> Error e
  | Ok m -> Ok (Metadata.is_file m)

let is_dir = fun path ->
  match metadata path with
  | Error e -> Error e
  | Ok m -> Ok (Metadata.is_dir m)

let current_dir = fun () ->
  match Env.current_dir () with
  | Ok cwd -> Ok cwd
  | Error _ -> Error (IO.Unknown_error "Failed to get current directory")
