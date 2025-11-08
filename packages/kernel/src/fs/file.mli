open Global0
  open IO

type seek_command = SeekSet | SeekCur | SeekEnd

type lock_command =
  | LockExclusive
  | LockShared
  | TryLockExclusive
  | TryLockShared
  | Unlock

val seek_command_to_unix : seek_command -> Unix.seek_command
val lock_command_to_unix : lock_command -> Unix.lock_command

type t = Fd.t

module Metadata : sig
  type t = Unix.stats

  val dev : t -> int
  val ino : t -> int
  val kind : t -> IO.file_kind
  val perm : t -> int
  val nlink : t -> int
  val uid : t -> int
  val gid : t -> int
  val rdev : t -> int
  val size : t -> int
  val atime : t -> float
  val mtime : t -> float
  val ctime : t -> float
end

val close : t -> unit

val read :
  t ->
  ?pos:int ->
  ?len:int ->
  bytes ->
  (int, error) result

val write :
  t ->
  ?pos:int ->
  ?len:int ->
  bytes ->
  (int, error) result

val read_vectored :
  t -> Iovec.t -> (int, error) result

val write_vectored :
  t -> Iovec.t -> (int, error) result

val sendfile :
  t ->
  file:Fd.t ->
  off:int ->
  len:int ->
  (int, error) result

val mkdir : string -> int -> (unit, error) result
val mkdirp : string -> int -> (unit, error) result
val copy_file : string -> string -> (unit, error) result
val is_directory : string -> (bool, error) result
val file_exists : string -> (bool, error) result
val stat : string -> (Metadata.t, error) result
val chmod : string -> int -> (unit, error) result
val symlink : string -> string -> (unit, error) result
val rmdir : string -> (unit, error) result
val remove : string -> (unit, error) result
val getcwd : unit -> (string, error) result
val chdir : string -> (unit, error) result
val is_regular_file : string -> (bool, error) result
val realpath : string -> (string, error) result
val link : string -> string -> (unit, error) result
val rename : string -> string -> (unit, error) result
val readlink : string -> (string, error) result
val fstat : t -> (Metadata.t, error) result
val lstat : string -> (Metadata.t, error) result

val lseek :
  t -> int64 -> seek_command -> (int64, error) result

val ftruncate : t -> int64 -> (unit, error) result
val fchmod : t -> int -> (unit, error) result
val fsync : t -> (unit, error) result
val dup : t -> (t, error) result

val lockf :
  t -> lock_command -> int -> (unit, error) result

val close_fd : t -> (unit, error) result
val get_temp_dir : unit -> (string, error) result

val temp_dir :
  ?temp_dir:string ->
  string ->
  string ->
  (string, error) result

val to_source : t -> Async.Source.t
