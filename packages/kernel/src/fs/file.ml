open Global0
open Collections
open IO
open Async

module Sys = Stdlib.Sys

type seek_command = SeekSet | SeekCur | SeekEnd

type lock_command =
  | LockExclusive
  | LockShared
  | TryLockExclusive
  | TryLockShared
  | Unlock

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

  let of_stats (stats : Unix.stats) = stats

  let dev t = t.Unix.st_dev
  let ino t = t.Unix.st_ino
  let kind t = IO.file_kind_of_unix t.Unix.st_kind
  let perm t = t.Unix.st_perm
  let nlink t = t.Unix.st_nlink
  let uid t = t.Unix.st_uid
  let gid t = t.Unix.st_gid
  let rdev t = t.Unix.st_rdev
  let size t = t.Unix.st_size
  let atime t = t.Unix.st_atime
  let mtime t = t.Unix.st_mtime
  let ctime t = t.Unix.st_ctime
end

let close fd = Fd.close fd

let read fd ?(pos = 0) ?len buf =
  let len = Option.unwrap_or len ~default:(Bytes.length buf - 1) in
  try Ok (UnixLabels.read (Fd.to_unix fd) ~buf ~pos ~len)
  with Unix.Unix_error (err, _, _) -> Error (IO.error_of_unix err)

let write fd ?(pos = 0) ?len buf =
  let len = Option.unwrap_or len ~default:(Bytes.length buf - 1) in
  try Ok (UnixLabels.write (Fd.to_unix fd) ~buf ~pos ~len)
  with Unix.Unix_error (err, _, _) -> Error (IO.error_of_unix err)

external std_sys_readv : Unix.file_descr -> IO.Iovec.t -> int = "kernel_unix_readv"

let read_vectored fd iov =
  try Ok (std_sys_readv (Fd.to_unix fd) iov)
  with Unix.Unix_error (err, _, _) -> Error (IO.error_of_unix err)

external std_sys_writev : Unix.file_descr -> IO.Iovec.t -> int
  = "kernel_unix_writev"

let write_vectored fd iov =
  try Ok (std_sys_writev (Fd.to_unix fd) iov)
  with Unix.Unix_error (err, _, _) -> Error (IO.error_of_unix err)

external std_sys_sendfile :
  Unix.file_descr -> Unix.file_descr -> int -> int -> int
  = "kernel_unix_sendfile"

external std_sys_copy_file : Unix.file_descr -> Unix.file_descr -> unit
  = "kernel_unix_copy_file"

let sendfile fd ~file ~off ~len =
  try Ok (std_sys_sendfile (Fd.to_unix file) (Fd.to_unix fd) off len)
  with Unix.Unix_error (err, _, _) -> Error (IO.error_of_unix err)

let mkdir path perm =
  try Ok (Unix.mkdir path perm)
  with Unix.Unix_error (err, _, _) -> Error (IO.error_of_unix err)

let mkdirp path perm =
  try
    (* Split path into components, handling absolute paths *)
    let components =
      let parts = String.split_on_char '/' path in
      let is_not_empty s = match s with "" -> false | _ -> true in
      match parts with
      | "" :: rest -> "/" :: List.filter is_not_empty rest
      | parts -> List.filter is_not_empty parts
    in
    (* Create each directory component incrementally *)
    let rec create_dirs current_path = function
      | [] -> ()
      | component :: rest ->
          let new_path =
            match (current_path, component) with
            | "", "/" -> "/"
            | "", c -> c
            | "/", c -> "/" ^ c
            | p, c -> p ^ "/" ^ c
          in
          (try Unix.mkdir new_path perm with
          | Unix.Unix_error (Unix.EEXIST, _, _) -> ());
          create_dirs new_path rest
    in
    create_dirs "" components;
    Ok ()
  with Unix.Unix_error (err, _, _) -> Error (IO.error_of_unix err)

let stat path =
  try Ok (Unix.stat path)
  with Unix.Unix_error (err, _, _) -> Error (IO.error_of_unix err)

let copy_file src dst =
  try
    let src_perms = Unix.(stat src).st_perm in
    let src_fd = Fd.open_file src [ Fd.OpenFlags.ReadOnly ] 0 in
    let dst_fd =
      Fd.open_file dst
        [ Fd.OpenFlags.WriteOnly; Fd.OpenFlags.Create; Fd.OpenFlags.Truncate ]
        src_perms
    in
    Fun.protect
      ~finally:(fun () ->
        Fd.close src_fd;
        Fd.close dst_fd)
      (fun () -> std_sys_copy_file (Fd.to_unix src_fd) (Fd.to_unix dst_fd));
    Ok ()
  with Unix.Unix_error (err, _, _) -> Error (IO.error_of_unix err)

let is_directory path =
  try Ok (Sys.is_directory path)
  with Unix.Unix_error (err, _, _) -> Error (IO.error_of_unix err)

let file_exists path =
  try Ok (Sys.file_exists path)
  with Unix.Unix_error (err, _, _) -> Error (IO.error_of_unix err)

let chmod path perm =
  try Ok (Unix.chmod path perm)
  with Unix.Unix_error (err, _, _) -> Error (IO.error_of_unix err)

let symlink src dst =
  try Ok (Unix.symlink src dst)
  with Unix.Unix_error (err, _, _) -> Error (IO.error_of_unix err)

let rmdir path =
  try Ok (Unix.rmdir path)
  with Unix.Unix_error (err, _, _) -> Error (IO.error_of_unix err)

let remove path =
  try Ok (Sys.remove path)
  with Unix.Unix_error (err, _, _) -> Error (IO.error_of_unix err)

let getcwd () =
  try Ok (Sys.getcwd ())
  with Unix.Unix_error (err, _, _) -> Error (IO.error_of_unix err)

let chdir path =
  try Ok (Sys.chdir path)
  with Unix.Unix_error (err, _, _) -> Error (IO.error_of_unix err)

let readdir path =
  try Ok (Sys.readdir path |> Array.to_list)
  with Unix.Unix_error (err, _, _) -> Error (IO.error_of_unix err)

let is_regular_file path =
  try
    let stats = Unix.stat path in
    Ok (stats.st_kind = Unix.S_REG)
  with Unix.Unix_error (err, _, _) -> Error (IO.error_of_unix err)

let realpath path =
  try Ok (Unix.realpath path)
  with Unix.Unix_error (err, _, _) -> Error (IO.error_of_unix err)

let link src dst =
  try Ok (Unix.link src dst)
  with Unix.Unix_error (err, _, _) -> Error (IO.error_of_unix err)

let rename src dst =
  try Ok (Unix.rename src dst)
  with Unix.Unix_error (err, _, _) -> Error (IO.error_of_unix err)

let readlink path =
  try Ok (Unix.readlink path)
  with Unix.Unix_error (err, _, _) -> Error (IO.error_of_unix err)

let fstat fd =
  try Ok (Metadata.of_stats (Unix.fstat (Fd.to_unix fd)))
  with Unix.Unix_error (err, _, _) -> Error (IO.error_of_unix err)

let lstat path =
  try Ok (Metadata.of_stats (Unix.lstat path))
  with Unix.Unix_error (err, _, _) -> Error (IO.error_of_unix err)

let lseek fd off cmd =
  try Ok (Int64.of_int (Unix.lseek (Fd.to_unix fd) (Int64.to_int off) (seek_command_to_unix cmd)))
  with Unix.Unix_error (err, _, _) -> Error (IO.error_of_unix err)

let ftruncate fd len =
  try Ok (Unix.ftruncate (Fd.to_unix fd) (Int64.to_int len))
  with Unix.Unix_error (err, _, _) -> Error (IO.error_of_unix err)

let fchmod fd perm =
  try Ok (Unix.fchmod (Fd.to_unix fd) perm)
  with Unix.Unix_error (err, _, _) -> Error (IO.error_of_unix err)

let fsync fd =
  try Ok (Unix.fsync (Fd.to_unix fd))
  with Unix.Unix_error (err, _, _) -> Error (IO.error_of_unix err)

let dup fd =
  try Ok (Fd.of_unix (Unix.dup (Fd.to_unix fd)))
  with Unix.Unix_error (err, _, _) -> Error (IO.error_of_unix err)

let lockf fd cmd len =
  try Ok (Unix.lockf (Fd.to_unix fd) (lock_command_to_unix cmd) len)
  with Unix.Unix_error (err, _, _) -> Error (IO.error_of_unix err)

let close_fd fd =
  try Ok (Fd.close fd)
  with Unix.Unix_error (err, _, _) -> Error (IO.error_of_unix err)

let get_temp_dir () =
  try Ok (Stdlib.Filename.get_temp_dir_name ())
  with Unix.Unix_error (err, _, _) -> Error (IO.error_of_unix err)

let temp_dir ?temp_dir prefix suffix =
  try
    let temp_parent = Option.unwrap_or temp_dir ~default:(Stdlib.Filename.get_temp_dir_name ()) in
    Ok (Stdlib.Filename.temp_dir ~temp_dir:temp_parent prefix suffix)
  with Unix.Unix_error (err, _, _) -> Error (IO.error_of_unix err)

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
