open Async

type open_flag =
  | ReadOnly
  | WriteOnly
  | ReadWrite
  | Create
  | Truncate
  | Append
  | Exclusive

type seek_command =
  | SeekSet
  | SeekCur
  | SeekEnd

type lock_command =
  | LockExclusive
  | LockShared
  | TryLockExclusive
  | TryLockShared
  | Unlock

let open_flags_to_unix flags =
  List.map (function
    | ReadOnly -> Unix.O_RDONLY
    | WriteOnly -> Unix.O_WRONLY
    | ReadWrite -> Unix.O_RDWR
    | Create -> Unix.O_CREAT
    | Truncate -> Unix.O_TRUNC
    | Append -> Unix.O_APPEND
    | Exclusive -> Unix.O_EXCL
  ) flags

let seek_command_to_unix = function
  | SeekSet -> Unix.SEEK_SET
  | SeekCur -> Unix.SEEK_CUR
  | SeekEnd -> Unix.SEEK_END

let lock_command_to_unix = function
  | LockExclusive -> Unix.F_LOCK
  | LockShared -> Unix.F_RLOCK
  | TryLockExclusive -> Unix.F_TLOCK
  | TryLockShared -> Unix.F_TRLOCK
  | Unlock -> Unix.F_ULOCK

type t = Fd.t

module Metadata = struct
  type t = Unix.stats

  let dev s = s.Unix.st_dev
  let ino s = s.Unix.st_ino
  let kind s = s.Unix.st_kind
  let perm s = s.Unix.st_perm
  let nlink s = s.Unix.st_nlink
  let uid s = s.Unix.st_uid
  let gid s = s.Unix.st_gid
  let rdev s = s.Unix.st_rdev
  let size s = s.Unix.st_size
  let atime s = s.Unix.st_atime
  let mtime s = s.Unix.st_mtime
  let ctime s = s.Unix.st_ctime
end

let pp = Fd.pp
let close = Fd.close

let read fd ?(pos = 0) ?len buf =
  let len = Option.value len ~default:(Bytes.length buf - 1) in
  syscall @@ fun () -> Ok (UnixLabels.read fd ~buf ~pos ~len)

let write fd ?(pos = 0) ?len buf =
  let len = Option.value len ~default:(Bytes.length buf - 1) in
  syscall @@ fun () -> Ok (UnixLabels.write fd ~buf ~pos ~len)

external std_sys_readv : Unix.file_descr -> Iovec.t -> int
  = "kernel_unix_readv"

let read_vectored fd iov = syscall @@ fun () -> Ok (std_sys_readv fd iov)

external std_sys_writev : Unix.file_descr -> Iovec.t -> int
  = "kernel_unix_writev"

let write_vectored fd iov = syscall @@ fun () -> Ok (std_sys_writev fd iov)

external std_sys_sendfile :
  Unix.file_descr -> Unix.file_descr -> int -> int -> int
  = "kernel_unix_sendfile"

let sendfile fd ~file ~off ~len =
  syscall @@ fun () -> Ok (std_sys_sendfile file fd off len)

let readdir path =
  syscall @@ fun () ->
  try Ok (Array.to_list (Sys.readdir path))
  with e -> Error (`Unix_error Unix.ENOENT)

let mkdir path perm =
  syscall @@ fun () ->
  try
    Unix.mkdir path perm;
    Ok ()
  with Unix.Unix_error (e, _, _) -> Error (`Unix_error e)

let mkdirp path perm =
  syscall @@ fun () ->
  (* Split path into components, handling absolute paths *)
  let components =
    let parts = String.split_on_char '/' path in
    match parts with
    | "" :: rest -> "/" :: List.filter (fun s -> s <> "") rest
    | parts -> List.filter (fun s -> s <> "") parts
  in
  (* Create each directory component incrementally using fold *)
  let create_dir acc_result component =
    match acc_result with
    | Error e -> Error e
    | Ok current_path -> (
        let new_path =
          match (current_path, component) with
          | "", "/" -> "/"
          | "", c -> c
          | "/", c -> "/" ^ c
          | p, c -> p ^ "/" ^ c
        in
        try
          Unix.mkdir new_path perm;
          Ok new_path
        with
        | Unix.Unix_error (Unix.EEXIST, _, _) -> Ok new_path
        | Unix.Unix_error (e, _, _) -> Error (`Unix_error e))
  in
  match List.fold_left create_dir (Ok "") components with
  | Ok _ -> Ok ()
  | Error e -> Error e

let copy_file src dst =
  syscall @@ fun () ->
  try
    let ic = open_in_bin src in
    let oc = open_out_bin dst in
    let buf_size = 65536 in
    (* 64KB buffer *)
    let buf = Bytes.create buf_size in
    let rec copy () =
      match input ic buf 0 buf_size with
      | 0 -> ()
      | n ->
          output oc buf 0 n;
          copy ()
    in
    Fun.protect
      ~finally:(fun () ->
        close_in_noerr ic;
        close_out_noerr oc)
      (fun () ->
        copy ();
        Ok ())
  with e -> Error (`Exn e)

let is_directory path =
  syscall @@ fun () ->
  try Ok (Sys.is_directory path) with e -> Error (`Exn e)

let file_exists path =
  syscall @@ fun () ->
  try Ok (Sys.file_exists path) with e -> Error (`Exn e)

let stat path =
  syscall @@ fun () ->
  try Ok (Unix.stat path)
  with Unix.Unix_error (e, _, _) -> Error (`Unix_error e)

let chmod path perm =
  syscall @@ fun () ->
  try
    Unix.chmod path perm;
    Ok ()
  with Unix.Unix_error (e, _, _) -> Error (`Unix_error e)

let symlink src dst =
  syscall @@ fun () ->
  try
    Unix.symlink src dst;
    Ok ()
  with Unix.Unix_error (e, _, _) -> Error (`Unix_error e)

let rmdir path =
  syscall @@ fun () ->
  try
    Unix.rmdir path;
    Ok ()
  with Unix.Unix_error (e, _, _) -> Error (`Unix_error e)

let remove path =
  syscall @@ fun () ->
  try
    Sys.remove path;
    Ok ()
  with e -> Error (`Exn e)

let getcwd () =
  syscall @@ fun () -> try Ok (Sys.getcwd ()) with e -> Error (`Exn e)

let chdir path =
  syscall @@ fun () ->
  try
    Sys.chdir path;
    Ok ()
  with e -> Error (`Exn e)

let opendir path =
  syscall @@ fun () ->
  try Ok (Unix.opendir path)
  with Unix.Unix_error (e, _, _) -> Error (`Unix_error e)

let readdir_handle handle =
  syscall @@ fun () ->
  try Ok (Unix.readdir handle) with
  | End_of_file -> Error `Eof
  | Unix.Unix_error (e, _, _) -> Error (`Unix_error e)

let closedir handle =
  syscall @@ fun () ->
  try
    Unix.closedir handle;
    Ok ()
  with Unix.Unix_error (e, _, _) -> Error (`Unix_error e)

let is_regular_file path =
  syscall @@ fun () ->
  try
    match Unix.stat path with
    | { st_kind = Unix.S_REG; _ } -> Ok true
    | _ -> Ok false
  with Unix.Unix_error (e, _, _) -> Error (`Unix_error e)

let realpath path =
  syscall @@ fun () ->
  try Ok (Unix.realpath path)
  with Unix.Unix_error (e, _, _) -> Error (`Unix_error e)

let link src dst =
  syscall @@ fun () ->
  try
    Unix.link src dst;
    Ok ()
  with Unix.Unix_error (e, _, _) -> Error (`Unix_error e)

let rename src dst =
  syscall @@ fun () ->
  try
    Unix.rename src dst;
    Ok ()
  with Unix.Unix_error (e, _, _) -> Error (`Unix_error e)

let readlink path =
  syscall @@ fun () ->
  try Ok (Unix.readlink path)
  with Unix.Unix_error (e, _, _) -> Error (`Unix_error e)

let open_file path flags perm =
  syscall @@ fun () ->
  try Ok (Unix.openfile path (open_flags_to_unix flags) perm)
  with Unix.Unix_error (e, _, _) -> Error (`Unix_error e)

let fstat fd =
  syscall @@ fun () ->
  try Ok (Unix.fstat fd)
  with Unix.Unix_error (e, _, _) -> Error (`Unix_error e)

let lstat path =
  syscall @@ fun () ->
  try Ok (Unix.lstat path)
  with Unix.Unix_error (e, _, _) -> Error (`Unix_error e)

let lseek fd pos whence =
  syscall @@ fun () ->
  try Ok (Unix.LargeFile.lseek fd pos (seek_command_to_unix whence))
  with Unix.Unix_error (e, _, _) -> Error (`Unix_error e)

let ftruncate fd len =
  syscall @@ fun () ->
  try
    Unix.LargeFile.ftruncate fd len;
    Ok ()
  with Unix.Unix_error (e, _, _) -> Error (`Unix_error e)

let fchmod fd perm =
  syscall @@ fun () ->
  try
    Unix.fchmod fd perm;
    Ok ()
  with Unix.Unix_error (e, _, _) -> Error (`Unix_error e)

let fsync fd =
  syscall @@ fun () ->
  try
    Unix.fsync fd;
    Ok ()
  with Unix.Unix_error (e, _, _) -> Error (`Unix_error e)

let dup fd =
  syscall @@ fun () ->
  try Ok (Unix.dup fd)
  with Unix.Unix_error (e, _, _) -> Error (`Unix_error e)

let lockf fd cmd len =
  syscall @@ fun () ->
  try
    Unix.lockf fd (lock_command_to_unix cmd) len;
    Ok ()
  with Unix.Unix_error (e, _, _) -> Error (`Unix_error e)

let close_fd fd =
  syscall @@ fun () ->
  try
    Unix.close fd;
    Ok ()
  with Unix.Unix_error (e, _, _) -> Error (`Unix_error e)

let get_temp_dir () =
  syscall @@ fun () ->
  try Ok (Filename.get_temp_dir_name ()) with e -> Error (`Exn e)

let temp_dir ?(temp_dir = Filename.get_temp_dir_name ()) prefix suffix =
  syscall @@ fun () ->
  try Ok (Filename.temp_dir ~temp_dir prefix suffix)
  with e -> Error (`Exn e)

let to_source t =
  let module Src = struct
    type nonrec t = t

    let register t selector token interest =
      Adapter.Selector.register selector ~fd:t ~token ~interest

    let reregister t selector token interest =
      Adapter.Selector.reregister selector ~fd:t ~token ~interest

    let deregister t selector = Adapter.Selector.deregister selector ~fd:t
  end in
  Source.make (module Src) t
