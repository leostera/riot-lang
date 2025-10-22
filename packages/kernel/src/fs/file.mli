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

val to_string : t -> string
val close : t -> unit

val read :
  t ->
  ?pos:int ->
  ?len:int ->
  bytes ->
  (int, [> Async.io_error ]) Async.io_result

val write :
  t ->
  ?pos:int ->
  ?len:int ->
  bytes ->
  (int, [> Async.io_error ]) Async.io_result

val read_vectored :
  t -> Async.Iovec.t -> (int, [> Async.io_error ]) Async.io_result

val write_vectored :
  t -> Async.Iovec.t -> (int, [> Async.io_error ]) Async.io_result

val sendfile :
  t ->
  file:Fd.t ->
  off:int ->
  len:int ->
  (int, [> Async.io_error ]) Async.io_result

val readdir : string -> (string list, [> Async.io_error ]) Async.io_result
val mkdir : string -> int -> (unit, [> Async.io_error ]) Async.io_result
val mkdirp : string -> int -> (unit, [> Async.io_error ]) Async.io_result
val copy_file : string -> string -> (unit, [> Async.io_error ]) Async.io_result
val is_directory : string -> (bool, [> Async.io_error ]) Async.io_result
val file_exists : string -> (bool, [> Async.io_error ]) Async.io_result
val stat : string -> (Metadata.t, [> Async.io_error ]) Async.io_result
val chmod : string -> int -> (unit, [> Async.io_error ]) Async.io_result
val symlink : string -> string -> (unit, [> Async.io_error ]) Async.io_result
val rmdir : string -> (unit, [> Async.io_error ]) Async.io_result
val remove : string -> (unit, [> Async.io_error ]) Async.io_result
val getcwd : unit -> (string, [> Async.io_error ]) Async.io_result
val chdir : string -> (unit, [> Async.io_error ]) Async.io_result
val is_regular_file : string -> (bool, [> Async.io_error ]) Async.io_result
val realpath : string -> (string, [> Async.io_error ]) Async.io_result
val link : string -> string -> (unit, [> Async.io_error ]) Async.io_result
val rename : string -> string -> (unit, [> Async.io_error ]) Async.io_result
val readlink : string -> (string, [> Async.io_error ]) Async.io_result
val fstat : t -> (Metadata.t, [> Async.io_error ]) Async.io_result
val lstat : string -> (Metadata.t, [> Async.io_error ]) Async.io_result

val lseek :
  t -> int64 -> seek_command -> (int64, [> Async.io_error ]) Async.io_result

val ftruncate : t -> int64 -> (unit, [> Async.io_error ]) Async.io_result
val fchmod : t -> int -> (unit, [> Async.io_error ]) Async.io_result
val fsync : t -> (unit, [> Async.io_error ]) Async.io_result
val dup : t -> (t, [> Async.io_error ]) Async.io_result

val lockf :
  t -> lock_command -> int -> (unit, [> Async.io_error ]) Async.io_result

val close_fd : t -> (unit, [> Async.io_error ]) Async.io_result
val get_temp_dir : unit -> (string, [> Async.io_error ]) Async.io_result

val temp_dir :
  ?temp_dir:string ->
  string ->
  string ->
  (string, [> Async.io_error ]) Async.io_result

val to_source : t -> Async.Source.t
