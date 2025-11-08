open Global0
open Collections
open IO

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

  let dev s = s.Unix.st_dev
  let ino s = s.Unix.st_ino
  let kind s = IO.file_kind_of_unix s.Unix.st_kind
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

let close = Fd.close

(* Use Async.syscall which now returns IO.error directly *)
let syscall = Async.syscall

let read fd ?(pos = 0) ?len buf =
  let len = Option.unwrap_or len ~default:(Bytes.length buf - 1) in
  syscall @@ fun () -> Ok (UnixLabels.read (Fd.to_unix fd) ~buf ~pos ~len)

let write fd ?(pos = 0) ?len buf =
  let len = Option.unwrap_or len ~default:(Bytes.length buf - 1) in
  syscall @@ fun () -> Ok (UnixLabels.write (Fd.to_unix fd) ~buf ~pos ~len)

external std_sys_readv : Unix.file_descr -> IO.Iovec.t -> int = "kernel_unix_readv"

let read_vectored fd iov =
  syscall @@ fun () -> Ok ((std_sys_readv (Fd.to_unix fd) iov))

external std_sys_writev : Unix.file_descr -> IO.Iovec.t -> int
  = "kernel_unix_writev"

let write_vectored fd iov =
  syscall @@ fun () -> Ok ((std_sys_writev (Fd.to_unix fd) iov))

external std_sys_sendfile :
  Unix.file_descr -> Unix.file_descr -> int -> int -> int
  = "kernel_unix_sendfile"

external std_sys_copy_file : Unix.file_descr -> Unix.file_descr -> unit
  = "kernel_unix_copy_file"

let sendfile fd ~file ~off ~len =
  syscall @@ fun () -> Ok (std_sys_sendfile (Fd.to_unix file) (Fd.to_unix fd) off len)

let mkdir path perm =
  syscall @@ fun () -> Ok (Unix.mkdir path perm)

let mkdirp path perm =
  syscall @@ fun () ->
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

let stat path =
  syscall @@ fun () -> Ok (Unix.stat path)

let copy_file src dst =
  syscall @@ fun () ->
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

let is_directory path =
  syscall @@ fun () -> Ok (Sys.is_directory path)

let file_exists path =
  syscall @@ fun () -> Ok (Sys.file_exists path)

let chmod path perm =
  syscall @@ fun () -> Ok (Unix.chmod path perm)

let symlink src dst =
  syscall @@ fun () -> Ok (Unix.symlink src dst)

let rmdir path =
  syscall @@ fun () -> Ok (Unix.rmdir path)

let remove path =
  syscall @@ fun () -> Ok (Sys.remove path)

let getcwd () =
  syscall @@ fun () -> Ok (Sys.getcwd ())

let chdir path =
  syscall @@ fun () -> Ok (Sys.chdir path)

let is_regular_file path =
  syscall @@ fun () ->
  match Unix.stat path with
  | { st_kind = Unix.S_REG; _ } -> Ok true
  | _ -> Ok false

let realpath path =
  syscall @@ fun () -> Ok (Unix.realpath path)

let link src dst =
  syscall @@ fun () -> Ok (Unix.link src dst)

let rename src dst =
  syscall @@ fun () -> Ok (Unix.rename src dst)

let readlink path =
  syscall @@ fun () -> Ok (Unix.readlink path)

let fstat fd =
  syscall @@ fun () -> Ok (Unix.fstat (Fd.to_unix fd))

let lstat path =
  syscall @@ fun () -> Ok (Unix.lstat path)

let lseek fd pos whence =
  syscall @@ fun () ->
  Ok (Unix.LargeFile.lseek (Fd.to_unix fd) pos (seek_command_to_unix whence))

let ftruncate fd len =
  syscall @@ fun () -> Ok (Unix.LargeFile.ftruncate (Fd.to_unix fd) len)

let fchmod fd perm =
  syscall @@ fun () -> Ok (Unix.fchmod (Fd.to_unix fd) perm)

let fsync fd =
  syscall @@ fun () -> Ok (Unix.fsync (Fd.to_unix fd))

let dup fd =
  syscall @@ fun () -> Ok (Fd.of_unix (Unix.dup (Fd.to_unix fd)))

let lockf fd cmd len =
  syscall @@ fun () -> Ok (Unix.lockf (Fd.to_unix fd) (lock_command_to_unix cmd) len)

let close_fd fd =
  syscall @@ fun () -> Ok (Unix.close (Fd.to_unix fd))

let get_temp_dir () =
  syscall @@ fun () -> 
    try Ok (Sys.getenv "TMPDIR") with Not_found ->
    try Ok (Sys.getenv "TEMP") with Not_found ->
    try Ok (Sys.getenv "TMP") with Not_found ->
    Ok "/tmp"

let temp_dir ?temp_dir prefix suffix =
  let base_dir = match temp_dir with
    | Some d -> d
    | None -> 
        try Sys.getenv "TMPDIR" with Not_found ->
        try Sys.getenv "TEMP" with Not_found ->
        try Sys.getenv "TMP" with Not_found ->
        "/tmp"
  in
  (* Generate a unique directory name *)
  let rec try_create attempt =
    if attempt > 1000 then
      panic "Could not create temporary directory after 1000 attempts"
    else
      let random_suffix = Stdlib.Random.int 0xFFFFFF in
      let dir_name = base_dir ^ "/" ^ prefix ^ string_of_int random_suffix ^ suffix in
      try
        Unix.mkdir dir_name 0o700;
        dir_name
      with
      | Unix.Unix_error (Unix.EEXIST, _, _) -> try_create (attempt + 1)
  in
  syscall @@ fun () -> Ok (try_create 0)

let to_source t =
  let module Src = struct
    type nonrec t = t

    let register t selector token interest =
      Async.Adapter.Selector.register selector ~fd:t ~token ~interest

    let reregister t selector token interest =
      Async.Adapter.Selector.reregister selector ~fd:t ~token ~interest

    let deregister t selector = Async.Adapter.Selector.deregister selector ~fd:t
  end in
  Async.Source.make (module Src) t
