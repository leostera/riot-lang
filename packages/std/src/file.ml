(** File types from Global.File *)
type kind = Regular | Directory | Character | Block | Link | Fifo | Socket

let kind_of_unix = function
  | Unix.S_REG -> Regular
  | Unix.S_DIR -> Directory
  | Unix.S_CHR -> Character
  | Unix.S_BLK -> Block
  | Unix.S_LNK -> Link
  | Unix.S_FIFO -> Fifo
  | Unix.S_SOCK -> Socket

type error =
  [ `File_not_found
  | `Permission_denied
  | `Is_a_directory
  | `Not_a_directory
  | `Already_exists
  | `No_space
  | `Unknown of string ]

let error_of_unix_error = function
  | Unix.ENOENT -> `File_not_found
  | Unix.EACCES | Unix.EPERM -> `Permission_denied
  | Unix.EISDIR -> `Is_a_directory
  | Unix.ENOTDIR -> `Not_a_directory
  | Unix.EEXIST -> `Already_exists
  | Unix.ENOSPC -> `No_space
  | e -> `Unknown (Unix.error_message e)

let exists ~path =
  match Unix.stat path with
  | exception Unix.Unix_error (Unix.ENOENT, _, _) -> false
  | _ -> true

let remove ~path =
  try
    Unix.unlink path;
    Ok ()
  with Unix.Unix_error (e, _, _) -> Error (error_of_unix_error e)

let read ~path =
  let open_flags = [ Unix.O_RDONLY ] in
  try
    let fd = Unix.openfile path open_flags 0o640 in
    let gluon_fd = fd in
    let source = Std_sys.IO.File.to_source gluon_fd in

    (* Get file size *)
    let stats = Unix.fstat fd in
    let size = stats.Unix.st_size in
    let buffer = Bytes.create size in

    let rec read_loop pos remaining =
      if remaining = 0 then Ok (Bytes.to_string buffer)
      else
        match Std_sys.IO.File.read gluon_fd buffer ~pos ~len:remaining with
        | Ok bytes_read ->
            if bytes_read = 0 then Ok (Bytes.sub_string buffer 0 pos)
            else read_loop (pos + bytes_read) (remaining - bytes_read)
        | Error `Would_block ->
            Miniriot.syscall ~name:"File.read" ~interest:Std_sys.IO.Interest.readable
              ~source (fun () -> read_loop pos remaining)
        | Error e ->
            Unix.close fd;
            Error
              (`Unknown
                 (Printf.sprintf "Read error: %s"
                    (match e with
                    | `Closed -> "closed"
                    | `Invalid_argument -> "invalid argument"
                    | `Would_block -> "would block"
                    | _ -> "unknown")))
    in

    let result = read_loop 0 size in
    Unix.close fd;
    result
  with Unix.Unix_error (e, _, _) -> Error (error_of_unix_error e)

let write ~path ~content =
  let open_flags = [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC ] in
  try
    let fd = Unix.openfile path open_flags 0o640 in
    let gluon_fd = fd in
    let source = Std_sys.IO.File.to_source gluon_fd in
    let buffer = Bytes.of_string content in
    let len = Bytes.length buffer in

    let rec write_loop pos remaining =
      if remaining = 0 then Ok ()
      else
        match Std_sys.IO.File.write gluon_fd buffer ~pos ~len:remaining with
        | Ok bytes_written ->
            write_loop (pos + bytes_written) (remaining - bytes_written)
        | Error `Would_block ->
            Miniriot.syscall ~name:"File.write" ~interest:Std_sys.IO.Interest.writable
              ~source (fun () -> write_loop pos remaining)
        | Error e ->
            Unix.close fd;
            Error
              (`Unknown
                 (Printf.sprintf "Write error: %s"
                    (match e with
                    | `Closed -> "closed"
                    | `Invalid_argument -> "invalid argument"
                    | `Would_block -> "would block"
                    | _ -> "unknown")))
    in

    let result = write_loop 0 len in
    Unix.close fd;
    result
  with Unix.Unix_error (e, _, _) -> Error (error_of_unix_error e)

let list_dir ~path =
  try
    let handle = Unix.opendir path in
    let files = ref [] in
    let rec read_loop () =
      try
        let entry = Unix.readdir handle in
        if entry <> "." && entry <> ".." then files := entry :: !files;
        (* Yield to allow other tasks to run during directory scanning *)
        Miniriot.yield ();
        read_loop ()
      with End_of_file ->
        Unix.closedir handle;
        Ok (List.rev !files)
    in
    read_loop ()
  with Unix.Unix_error (e, _, _) -> Error (error_of_unix_error e)

let list_dir_all ~path = list_dir ~path

let is_directory ~path =
  try
    let stats = Unix.stat path in
    stats.Unix.st_kind = Unix.S_DIR
  with Unix.Unix_error (Unix.ENOENT, _, _) -> false

let readdir ~path =
  match Std_sys.IO.File.readdir path with
  | Ok files -> Ok files
  | Error (`Unix_error e) -> Error (error_of_unix_error e)
  | Error _ -> Error (`Unknown "Failed to read directory")

let mkdir ~path ~perm =
  match Std_sys.IO.File.mkdir path perm with
  | Ok () -> Ok ()
  | Error (`Unix_error e) -> Error (error_of_unix_error e)
  | Error _ -> Error (`Unknown "Failed to create directory")

let mkdirp ~path ~perm =
  match Std_sys.IO.File.mkdirp path perm with
  | Ok () -> Ok ()
  | Error (`Unix_error e) -> Error (error_of_unix_error e)
  | Error _ -> Error (`Unknown "Failed to create directory")

let copy_file ~src ~dst =
  match Std_sys.IO.File.copy_file src dst with
  | Ok () -> Ok ()
  | Error (`Unix_error e) -> Error (error_of_unix_error e)
  | Error (`Exn exn) -> Error (`Unknown (Printexc.to_string exn))
  | Error _ -> Error (`Unknown "Failed to copy file")

let file_exists ~path =
  match Std_sys.IO.File.file_exists path with
  | Ok exists -> Ok exists
  | Error _ -> Ok false

let stat ~path =
  match Std_sys.IO.File.stat path with
  | Ok stats -> Ok stats
  | Error (`Unix_error e) -> Error (error_of_unix_error e)
  | Error _ -> Error (`Unknown "Failed to stat file")

let chmod ~path ~perm =
  match Std_sys.IO.File.chmod path perm with
  | Ok () -> Ok ()
  | Error (`Unix_error e) -> Error (error_of_unix_error e)
  | Error _ -> Error (`Unknown "Failed to change permissions")

let symlink ~src ~dst =
  match Std_sys.IO.File.symlink src dst with
  | Ok () -> Ok ()
  | Error (`Unix_error e) -> Error (error_of_unix_error e)
  | Error _ -> Error (`Unknown "Failed to create symlink")

let rmdir ~path =
  match Std_sys.IO.File.rmdir path with
  | Ok () -> Ok ()
  | Error (`Unix_error e) -> Error (error_of_unix_error e)
  | Error _ -> Error (`Unknown "Failed to remove directory")

let getcwd () =
  match Std_sys.IO.File.getcwd () with
  | Ok path -> Ok path
  | Error _ -> Error (`Unknown "Failed to get current directory")

let chdir ~path =
  match Std_sys.IO.File.chdir path with
  | Ok () -> Ok ()
  | Error _ -> Error (`Unknown "Failed to change directory")

let opendir ~path =
  match Std_sys.IO.File.opendir path with
  | Ok handle -> Ok handle
  | Error (`Unix_error e) -> Error (error_of_unix_error e)
  | Error _ -> Error (`Unknown "Failed to open directory")

let readdir_handle ~handle =
  match Std_sys.IO.File.readdir_handle handle with
  | Ok entry -> Ok entry
  | Error `Eof -> Error (`Unknown "End of directory")
  | Error (`Unix_error e) -> Error (error_of_unix_error e)
  | Error _ -> Error (`Unknown "Failed to read directory")

let closedir ~handle =
  match Std_sys.IO.File.closedir handle with
  | Ok () -> Ok ()
  | Error (`Unix_error e) -> Error (error_of_unix_error e)
  | Error _ -> Error (`Unknown "Failed to close directory")
