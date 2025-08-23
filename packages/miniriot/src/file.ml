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
    let source = Gluon.File.to_source gluon_fd in

    (* Get file size *)
    let stats = Unix.fstat fd in
    let size = stats.Unix.st_size in
    let buffer = Bytes.create size in

    let rec read_loop pos remaining =
      if remaining = 0 then Ok (Bytes.to_string buffer)
      else
        match Gluon.File.read gluon_fd buffer ~pos ~len:remaining with
        | Ok bytes_read ->
            if bytes_read = 0 then Ok (Bytes.sub_string buffer 0 pos)
            else read_loop (pos + bytes_read) (remaining - bytes_read)
        | Error `Would_block ->
            Effects.syscall ~name:"File.read" ~interest:Gluon.Interest.readable
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
    let source = Gluon.File.to_source gluon_fd in
    let buffer = Bytes.of_string content in
    let len = Bytes.length buffer in

    let rec write_loop pos remaining =
      if remaining = 0 then Ok ()
      else
        match Gluon.File.write gluon_fd buffer ~pos ~len:remaining with
        | Ok bytes_written ->
            write_loop (pos + bytes_written) (remaining - bytes_written)
        | Error `Would_block ->
            Effects.syscall ~name:"File.write" ~interest:Gluon.Interest.writable
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
        Effects.yield ();
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
  match Gluon.File.readdir path with
  | Ok files -> Ok files
  | Error (`Unix_error e) -> Error (error_of_unix_error e)
  | Error _ -> Error (`Unknown "Failed to read directory")

let mkdir ~path ~perm =
  match Gluon.File.mkdir path perm with
  | Ok () -> Ok ()
  | Error (`Unix_error e) -> Error (error_of_unix_error e)
  | Error _ -> Error (`Unknown "Failed to create directory")

let mkdirp ~path ~perm =
  match Gluon.File.mkdirp path perm with
  | Ok () -> Ok ()
  | Error (`Unix_error e) -> Error (error_of_unix_error e)
  | Error _ -> Error (`Unknown "Failed to create directory")

let copy_file ~src ~dst =
  match Gluon.File.copy_file src dst with
  | Ok () -> Ok ()
  | Error (`Unix_error e) -> Error (error_of_unix_error e)
  | Error (`Exn exn) -> Error (`Unknown (Printexc.to_string exn))
  | Error _ -> Error (`Unknown "Failed to copy file")
