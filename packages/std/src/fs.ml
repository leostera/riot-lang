(** Filesystem utilities *)

type error = SystemError of string

(** Basic filesystem operations - defined first as they're used by other functions *)

let is_directory path =
  let path_str = Path.to_string path in
  try Ok (Sys.is_directory path_str)
  with e -> Error (SystemError (Printexc.to_string e))

let is_regular_file path =
  let path_str = Path.to_string path in
  try Ok ((Unix.stat path_str).st_kind = Unix.S_REG) with _ -> Ok false

let remove_file path =
  let path_str = Path.to_string path in
  try
    Sys.remove path_str;
    Ok ()
  with e -> Error (SystemError (Printexc.to_string e))

let rmdir path =
  let path_str = Path.to_string path in
  try
    Unix.rmdir path_str;
    Ok ()
  with e -> Error (SystemError (Printexc.to_string e))

let opendir path =
  let path_str = Path.to_string path in
  try Ok (Unix.opendir path_str)
  with e -> Error (SystemError (Printexc.to_string e))

let readdir_handle handle =
  try Ok (Unix.readdir handle) with
  | End_of_file -> Error (SystemError "End of directory")
  | e -> Error (SystemError (Printexc.to_string e))

let closedir handle =
  try
    Unix.closedir handle;
    Ok ()
  with e -> Error (SystemError (Printexc.to_string e))

let readdir path =
  match opendir path with
  | Error e -> Error e
  | Ok handle ->
      let rec read_all acc =
        match readdir_handle handle with
        | Error _ -> List.rev acc  (* End of directory or error *)
        | Ok entry ->
            if entry = "." || entry = ".." then
              read_all acc
            else
              read_all (entry :: acc)
      in
      let entries = read_all [] in
      match closedir handle with
      | Error e -> Error e
      | Ok () -> Ok entries

(** Directory reading iterator *)
module ReadDir = struct
  type t = {
    handle : Unix.dir_handle;
    mutable closed : bool;
  }

  let create path =
    match opendir path with
    | Error e -> Error e
    | Ok handle -> Ok { handle; closed = false }

  let rec next t =
    if t.closed then None
    else
      try
        let entry = Unix.readdir t.handle in
        if entry = "." || entry = ".." then
          next t  (* Skip . and .. *)
        else
          match Path.of_string entry with
          | Ok p -> Some p
          | Error _ -> next t  (* Skip invalid paths *)
      with End_of_file ->
        t.closed <- true;
        None

  let close t =
    if not t.closed then (
      t.closed <- true;
      try
        Unix.closedir t.handle;
        Ok ()
      with e -> Error (SystemError (Printexc.to_string e))
    ) else Ok ()
end

(** Clean API implementations following the FIXME guidelines *)

let canonicalize path =
  let path_str = Path.to_string path in
  try
    let abs_path = Unix.realpath path_str in
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
        copy_loop ()
      )
    in
    copy_loop ();
    close_in ic;
    close_out oc;
    (* Copy permissions *)
    let stats = Unix.stat src_str in
    Unix.chmod dst_str stats.st_perm;
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
          | Ok () ->
              let parent_str = Path.to_string parent in
              try
                Unix.mkdir parent_str 0o755;
                Ok ()
              with
              | Unix.Unix_error (Unix.EEXIST, _, _) -> Ok ()
              | e -> Error (SystemError (Printexc.to_string e))
        else Ok ()
  in
  match create_parents path with
  | Error e -> Error e
  | Ok () ->
      let path_str = Path.to_string path in
      try
        Unix.mkdir path_str 0o755;
        Ok ()
      with
      | Unix.Unix_error (Unix.EEXIST, _, _) -> Ok ()
      | e -> Error (SystemError (Printexc.to_string e))

let exists path =
  Ok (Path.exists path)

let hard_link ~src ~dst =
  let src_str = Path.to_string src in
  let dst_str = Path.to_string dst in
  try
    Unix.link src_str dst_str;
    Ok ()
  with e -> Error (SystemError (Printexc.to_string e))

let metadata path =
  let path_str = Path.to_string path in
  try Ok (Unix.stat path_str)
  with e -> Error (SystemError (Printexc.to_string e))

let read_to_string path =
  let path_str = Path.to_string path in
  try
    let ic = open_in_bin path_str in
    let len = in_channel_length ic in
    let buf = really_input_string ic len in
    close_in ic;
    Ok buf
  with e -> Error (SystemError (Printexc.to_string e))

let read_dir path = ReadDir.create path

let remove_dir_all path =
  let rec remove_recursive path =
    match is_directory path with
    | Error e -> Error e
    | Ok false ->
        (* It's a file *)
        remove_file path
    | Ok true ->
        (* It's a directory *)
        match readdir path with
        | Error e -> Error e
        | Ok entries ->
            let rec remove_entries = function
              | [] -> rmdir path
              | entry :: rest ->
                  match Path.of_string entry with
                  | Error _ -> Error (SystemError ("Invalid path: " ^ entry))
                  | Ok entry_path ->
                      let full_path = Path.join path entry_path in
                      match remove_recursive full_path with
                      | Error e -> Error e
                      | Ok () -> remove_entries rest
            in
            remove_entries entries
  in
  remove_recursive path

let rename ~src ~dst =
  let src_str = Path.to_string src in
  let dst_str = Path.to_string dst in
  try
    Unix.rename src_str dst_str;
    Ok ()
  with e -> Error (SystemError (Printexc.to_string e))

let set_permissions path perm =
  let path_str = Path.to_string path in
  try
    Unix.chmod path_str perm;
    Ok ()
  with e -> Error (SystemError (Printexc.to_string e))

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
    let target = Unix.readlink path_str in
    match Path.of_string target with
    | Ok p -> Ok p
    | Error _ -> Error (SystemError "Invalid link target")
  with e -> Error (SystemError (Printexc.to_string e))

let create_dir path =
  let path_str = Path.to_string path in
  try
    if not (Sys.file_exists path_str) then Unix.mkdir path_str 0o755;
    Ok ()
  with e -> Error (SystemError (Printexc.to_string e))

let file_exists path =
  let path_str = Path.to_string path in
  try Ok (Sys.file_exists path_str)
  with e -> Error (SystemError (Printexc.to_string e))

let dir_exists path =
  let path_str = Path.to_string path in
  try Ok (Sys.file_exists path_str && Sys.is_directory path_str)
  with e -> Error (SystemError (Printexc.to_string e))

let stat path =
  let path_str = Path.to_string path in
  try Ok (Unix.stat path_str)
  with e -> Error (SystemError (Printexc.to_string e))

let chmod path perm =
  let path_str = Path.to_string path in
  try
    Unix.chmod path_str perm;
    Ok ()
  with e -> Error (SystemError (Printexc.to_string e))

let symlink src dst =
  let src_str = Path.to_string src in
  let dst_str = Path.to_string dst in
  try
    Unix.symlink src_str dst_str;
    Ok ()
  with e -> Error (SystemError (Printexc.to_string e))

let mkdir path perm =
  let path_str = Path.to_string path in
  try
    Unix.mkdir path_str perm;
    Ok ()
  with e -> Error (SystemError (Printexc.to_string e))

let mkdir_safe path perm =
  let path_str = Path.to_string path in
  try
    Unix.mkdir path_str perm;
    Ok ()
  with
  | Unix.Unix_error (Unix.EEXIST, _, _) -> Ok ()
  | e -> Error (SystemError (Printexc.to_string e))

let rec mkdirp path =
  let path_str = Path.to_string path in
  if not (Sys.file_exists path_str) then
    match Path.parent path with
    | Some parent ->
        let _ = mkdirp parent in
        mkdir_safe path 0o755
    | None -> mkdir_safe path 0o755
  else Ok ()

let rec remove_dir path =
  let path_str = Path.to_string path in
  try
    let handle = Unix.opendir path_str in
    (try
       while true do
         let file = Unix.readdir handle in
         if file <> "." && file <> ".." then
           let file_path =
             Path.join path
               (Path.of_string file |> Result.expect ~msg:"Invalid file path")
           in
           if Sys.is_directory (Path.to_string file_path) then
             let _ = remove_dir file_path in
             ()
           else Sys.remove (Path.to_string file_path)
       done
     with End_of_file -> ());
    Unix.closedir handle;
    Unix.rmdir path_str;
    Ok ()
  with e -> Error (SystemError (Printexc.to_string e))

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
  try
    match Path.of_string (Sys.getcwd ()) with
    | Ok path -> Ok path
    | Error path_error -> Error (SystemError "Invalid path")
  with e -> Error (SystemError (Printexc.to_string e))

let chdir path =
  try
    Sys.chdir (Path.to_string path);
    Ok ()
  with e -> Error (SystemError (Printexc.to_string e))

let temp_dir () =
  try
    match Path.of_string (Filename.get_temp_dir_name ()) with
    | Ok path -> Ok path
    | Error _ -> Error (SystemError "Invalid temp directory path")
  with e -> Error (SystemError (Printexc.to_string e))

let home_dir () =
  try
    match Sys.getenv_opt "HOME" with
    | Some home -> (
        match Path.of_string home with
        | Ok path -> Ok path
        | Error _ -> Error (SystemError "Invalid home directory path"))
    | None -> Error (SystemError "HOME environment variable not set")
  with e -> Error (SystemError (Printexc.to_string e))

let file_size path =
  let path_str = Path.to_string path in
  try Ok (Unix.stat path_str).st_size
  with e -> Error (SystemError (Printexc.to_string e))

let path_separator () = if Sys.unix then "/" else "\\"

let current_executable () =
  try
    match Path.of_string Sys.executable_name with
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
            match Path.of_string entry with
            | Ok p -> Some p
            | Error _ -> None)
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
            | entry :: rest ->
                let full_path =
                  Path.join path
                    (Path.of_string entry |> Result.expect ~msg:"Invalid path")
                in
                match walk full_path fn with
                | Error e -> Error e
                | Ok () -> walk_entries rest
          in
          walk_entries (List.filter (fun e -> e <> "." && e <> "..") entries))

