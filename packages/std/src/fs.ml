(** Filesystem utilities *)

type error = SystemError of string

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

let read_file path =
  let path_str = Path.to_string path in
  try
    let ic = open_in path_str in
    let len = in_channel_length ic in
    let content = really_input_string ic len in
    close_in ic;
    Ok content
  with e -> Error (SystemError (Printexc.to_string e))

let write_file path content =
  let path_str = Path.to_string path in
  try
    let oc = open_out path_str in
    output_string oc content;
    close_out oc;
    Ok ()
  with e -> Error (SystemError (Printexc.to_string e))

let remove_file path =
  let path_str = Path.to_string path in
  try
    Sys.remove path_str;
    Ok ()
  with e -> Error (SystemError (Printexc.to_string e))

let is_directory path =
  let path_str = Path.to_string path in
  try Ok (Sys.is_directory path_str)
  with e -> Error (SystemError (Printexc.to_string e))

let is_regular_file path =
  let path_str = Path.to_string path in
  try Ok ((Unix.stat path_str).st_kind = Unix.S_REG) with _ -> Ok false

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
  let path_str = Path.to_string path in
  try Ok (Array.to_list (Sys.readdir path_str))
  with e -> Error (SystemError (Printexc.to_string e))

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
  let path_str = Path.to_string path in
  try
    Sys.chdir path_str;
    Ok ()
  with e -> Error (SystemError (Printexc.to_string e))

let with_tempdir ?(prefix = "tmp") f =
  (* Create a unique temporary file to get a unique name *)
  let temp_base =
    try Filename.temp_file prefix ""
    with e ->
      failwith
        (Printf.sprintf "Failed to create temp file: %s" (Printexc.to_string e))
  in

  (* Convert to path and remove the file *)
  let temp_path =
    match Path.of_string temp_base with
    | Ok p -> p
    | Error _ -> failwith (Printf.sprintf "Invalid temp path: %s" temp_base)
  in

  (* Remove the file created by temp_file *)
  let _ = try Unix.unlink temp_base with _ -> () in

  (* Create the directory *)
  match create_dir temp_path with
  | Error e -> Error e
  | Ok () -> (
      (* Run the function with proper cleanup *)
      try
        let result = f temp_path in
        (* Clean up the directory *)
        let _ = remove_dir temp_path in
        Ok result
      with e ->
        (* Clean up even if the function fails *)
        let _ = remove_dir temp_path in
        Error
          (SystemError
             (Printf.sprintf "Function failed: %s" (Printexc.to_string e))))
