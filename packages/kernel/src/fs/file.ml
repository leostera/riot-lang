open Global0
open Collections
open IO
open Async
module Sys = Stdlib.Sys

type seek_command =
  SeekSet
  | SeekCur
  | SeekEnd

type lock_command =
  | LockExclusive
  | LockShared
  | TryLockExclusive
  | TryLockShared
  | Unlock

let seek_command_to_unix =
  function
  | SeekSet -> Unix.SEEK_SET
  | SeekCur -> Unix.SEEK_CUR
  | SeekEnd -> Unix.SEEK_END

let lock_command_to_unix =
  function
  | LockExclusive -> Unix.F_LOCK
  | LockShared -> Unix.F_RLOCK
  | TryLockExclusive -> Unix.F_TLOCK
  | TryLockShared -> Unix.F_TRLOCK
  | Unlock -> Unix.F_ULOCK

type t = Fd.t

module Metadata = struct
  type t = Unix.stats

  let of_stats = fun (stats: Unix.stats) -> stats

  let dev = fun t -> t.Unix.st_dev

  let ino = fun t -> t.Unix.st_ino

  let kind = fun t -> IO.file_kind_of_unix t.Unix.st_kind

  let perm = fun t -> t.Unix.st_perm

  let nlink = fun t -> t.Unix.st_nlink

  let uid = fun t -> t.Unix.st_uid

  let gid = fun t -> t.Unix.st_gid

  let rdev = fun t -> t.Unix.st_rdev

  let size = fun t -> t.Unix.st_size

  let atime = fun t -> t.Unix.st_atime

  let mtime = fun t -> t.Unix.st_mtime

  let ctime = fun t -> t.Unix.st_ctime
end

let close = fun fd -> Fd.close fd

let read = fun fd ?(pos = 0) ?len buf ->
  let len = Option.unwrap_or len ~default:(((((Bytes.length buf - 1))))) in
  IO.unix_syscall (fun () -> UnixLabels.read (Fd.to_unix fd) ~buf ~pos ~len)

let write = fun fd ?(pos = 0) ?len buf ->
  let len = Option.unwrap_or len ~default:(((((Bytes.length buf - 1))))) in
  IO.unix_syscall (fun () -> UnixLabels.write (Fd.to_unix fd) ~buf ~pos ~len)

external std_sys_readv: Unix.file_descr -> IO.Iovec.t -> int = "kernel_unix_readv"

let read_vectored = fun fd iov -> IO.unix_syscall (fun () -> std_sys_readv (Fd.to_unix fd) iov)

external std_sys_writev: Unix.file_descr -> IO.Iovec.t -> int = "kernel_unix_writev"

let write_vectored = fun fd iov -> IO.unix_syscall (fun () -> std_sys_writev (Fd.to_unix fd) iov)

external std_sys_sendfile: Unix.file_descr -> Unix.file_descr -> int -> int -> int = "kernel_unix_sendfile"

external std_sys_copy_file: Unix.file_descr -> Unix.file_descr -> unit = "kernel_unix_copy_file"

let sendfile = fun fd ~file ~off ~len -> IO.unix_syscall
(fun () -> std_sys_sendfile (Fd.to_unix file) (Fd.to_unix fd) off len)

let mkdir = fun path perm ->
  IO.unix_syscall
    (fun () ->
      Unix.mkdir path perm)

let mkdirp = fun path perm ->
  IO.unix_syscall
    (fun () ->
      (* Split path into components, handling absolute paths *)
      let components =
        let parts = String.split_on_char '/' path in
        let is_not_empty = fun s ->
          match s with
          | "" -> false
          | _ -> true
        in
        match parts with
        | "" :: rest -> "/" :: List.filter is_not_empty rest
        | parts -> List.filter is_not_empty parts
      in
      (* Create each directory component incrementally *)
      let rec create_dirs = fun current_path ->
        function
        | [] -> ()
        | component :: rest ->
            let new_path =
              match (current_path, component) with
              | "", "/" -> "/"
              | "", c -> c
              | "/", c -> "/" ^ c
              | p, c -> p ^ "/" ^ c
            in
            (
              try Unix.mkdir new_path perm with
              | Unix.Unix_error (Unix.EEXIST, _, _) -> ()
            );
            create_dirs new_path rest
      in
      create_dirs "" components)

let stat = fun path -> IO.unix_syscall (fun () -> Unix.stat path)

let copy_file = fun src dst ->
  IO.unix_syscall
    (fun () ->
      let src_perms = Unix.(stat src).st_perm in
      let src_fd = Fd.open_file src [ Fd.OpenFlags.ReadOnly ] 0 in
      let dst_fd = Fd.open_file
      dst
      [ Fd.OpenFlags.WriteOnly; Fd.OpenFlags.Create; Fd.OpenFlags.Truncate ]
      src_perms in
      Fun.protect
        ~finally:(fun () ->
          Fd.close src_fd;
          Fd.close dst_fd)
        (fun () -> std_sys_copy_file (Fd.to_unix src_fd) (Fd.to_unix dst_fd)))

let is_directory = fun path -> IO.unix_syscall (fun () -> Sys.is_directory path)

let file_exists = fun path -> IO.unix_syscall (fun () -> Sys.file_exists path)

let chmod = fun path perm ->
  IO.unix_syscall
    (fun () ->
      Unix.chmod path perm)

let symlink = fun src dst ->
  IO.unix_syscall
    (fun () ->
      Unix.symlink src dst)

let rmdir = fun path -> IO.unix_syscall (fun () -> Unix.rmdir path)

let remove = fun path -> IO.unix_syscall (fun () -> Sys.remove path)

let getcwd = fun () ->
  try Ok (Sys.getcwd ()) with
  | Unix.Unix_error (err, _, _) -> Error (IO.error_of_unix err)

let chdir = fun path ->
  try Ok (Sys.chdir path) with
  | Unix.Unix_error (err, _, _) -> Error (IO.error_of_unix err)

let readdir = fun path ->
  try Ok (Sys.readdir path |> Array.to_list) with
  | Unix.Unix_error (err, _, _) -> Error (IO.error_of_unix err)

let is_regular_file = fun path ->
  try
    let stats = Unix.stat path in
    Ok (stats.st_kind = Unix.S_REG)
  with
  | Unix.Unix_error (err, _, _) -> Error (IO.error_of_unix err)

let realpath = fun path ->
  try Ok (Unix.realpath path) with
  | Unix.Unix_error (err, _, _) -> Error (IO.error_of_unix err)

let link = fun src dst ->
  try Ok (Unix.link src dst) with
  | Unix.Unix_error (err, _, _) -> Error (IO.error_of_unix err)

let rename = fun src dst ->
  try Ok (Unix.rename src dst) with
  | Unix.Unix_error (err, _, _) -> Error (IO.error_of_unix err)

let readlink = fun path ->
  try Ok (Unix.readlink path) with
  | Unix.Unix_error (err, _, _) -> Error (IO.error_of_unix err)

let fstat = fun fd ->
  try Ok (Metadata.of_stats (Unix.fstat (Fd.to_unix fd))) with
  | Unix.Unix_error (err, _, _) -> Error (IO.error_of_unix err)

let lstat = fun path ->
  try Ok (Metadata.of_stats (Unix.lstat path)) with
  | Unix.Unix_error (err, _, _) -> Error (IO.error_of_unix err)

let lseek = fun fd off cmd ->
  try Ok (Int64.of_int (Unix.lseek (Fd.to_unix fd) (Int64.to_int off) (seek_command_to_unix cmd))) with
  | Unix.Unix_error (err, _, _) -> Error (IO.error_of_unix err)

let ftruncate = fun fd len ->
  try Ok (Unix.ftruncate (Fd.to_unix fd) (Int64.to_int len)) with
  | Unix.Unix_error (err, _, _) -> Error (IO.error_of_unix err)

let fchmod = fun fd perm ->
  try Ok (Unix.fchmod (Fd.to_unix fd) perm) with
  | Unix.Unix_error (err, _, _) -> Error (IO.error_of_unix err)

let fsync = fun fd ->
  try Ok (Unix.fsync (Fd.to_unix fd)) with
  | Unix.Unix_error (err, _, _) -> Error (IO.error_of_unix err)

let dup = fun fd ->
  try Ok (Fd.of_unix (Unix.dup (Fd.to_unix fd))) with
  | Unix.Unix_error (err, _, _) -> Error (IO.error_of_unix err)

let lockf = fun fd cmd len ->
  try Ok (Unix.lockf (Fd.to_unix fd) (lock_command_to_unix cmd) len) with
  | Unix.Unix_error (err, _, _) -> Error (IO.error_of_unix err)

let close_fd = fun fd ->
  try Ok (Fd.close fd) with
  | Unix.Unix_error (err, _, _) -> Error (IO.error_of_unix err)

let get_temp_dir = fun () ->
  try Ok (Stdlib.Filename.get_temp_dir_name ()) with
  | Unix.Unix_error (err, _, _) -> Error (IO.error_of_unix err)

let temp_dir = fun ?temp_dir prefix suffix ->
  try
    let temp_parent = Option.unwrap_or temp_dir ~default:(Stdlib.Filename.get_temp_dir_name ()) in
    Ok (Stdlib.Filename.temp_dir ~temp_dir:temp_parent prefix suffix)
  with
  | Unix.Unix_error (err, _, _) -> Error (IO.error_of_unix err)

let to_source = fun t ->
  let module Src = struct
    type nonrec t = t

    let register = fun t selector token interest -> Adapter.Selector.register
    selector
    ~fd:t
    ~token
    ~interest

    let reregister = fun t selector token interest -> Adapter.Selector.reregister
    selector
    ~fd:t
    ~token
    ~interest

    let deregister = fun t selector -> Adapter.Selector.deregister selector ~fd:t
  end in
  Source.make (module Src) t
