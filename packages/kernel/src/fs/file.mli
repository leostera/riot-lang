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

val open_flags_to_unix : open_flag list -> Unix.open_flag list
val seek_command_to_unix : seek_command -> Unix.seek_command
val lock_command_to_unix : lock_command -> Unix.lock_command

type t = Async.Fd.t

module Metadata : sig
  type t = Unix.stats

  val dev : t -> int
  val ino : t -> int
  val kind : t -> Unix.file_kind
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

val pp : Format.formatter -> t -> unit
val close : t -> unit
val read : t -> ?pos:int -> ?len:int -> bytes -> (int, [> Async.io_error ]) Async.io_result
val write : t -> ?pos:int -> ?len:int -> bytes -> (int, [> Async.io_error ]) Async.io_result
val read_vectored : t -> Async.Iovec.t -> (int, [> Async.io_error ]) Async.io_result
val write_vectored : t -> Async.Iovec.t -> (int, [> Async.io_error ]) Async.io_result
val sendfile : t -> file:Async.Fd.t -> off:int -> len:int -> (int, [> Async.io_error ]) Async.io_result
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
val opendir : string -> (Unix.dir_handle, [> Async.io_error ]) Async.io_result
val readdir_handle : Unix.dir_handle -> (string, [> Async.io_error ]) Async.io_result
val closedir : Unix.dir_handle -> (unit, [> Async.io_error ]) Async.io_result
val is_regular_file : string -> (bool, [> Async.io_error ]) Async.io_result
val realpath : string -> (string, [> Async.io_error ]) Async.io_result
val link : string -> string -> (unit, [> Async.io_error ]) Async.io_result
val rename : string -> string -> (unit, [> Async.io_error ]) Async.io_result
val readlink : string -> (string, [> Async.io_error ]) Async.io_result
val open_file : string -> open_flag list -> int -> (Unix.file_descr, [> Async.io_error ]) Async.io_result
val fstat : Unix.file_descr -> (Metadata.t, [> Async.io_error ]) Async.io_result
val lstat : string -> (Metadata.t, [> Async.io_error ]) Async.io_result
val lseek : Unix.file_descr -> int64 -> seek_command -> (int64, [> Async.io_error ]) Async.io_result
val ftruncate : Unix.file_descr -> int64 -> (unit, [> Async.io_error ]) Async.io_result
val fchmod : Unix.file_descr -> int -> (unit, [> Async.io_error ]) Async.io_result
val fsync : Unix.file_descr -> (unit, [> Async.io_error ]) Async.io_result
val dup : Unix.file_descr -> (Unix.file_descr, [> Async.io_error ]) Async.io_result
val lockf : Unix.file_descr -> lock_command -> int -> (unit, [> Async.io_error ]) Async.io_result
val close_fd : Unix.file_descr -> (unit, [> Async.io_error ]) Async.io_result
val get_temp_dir : unit -> (string, [> Async.io_error ]) Async.io_result
val temp_dir : ?temp_dir:string -> string -> string -> (string, [> Async.io_error ]) Async.io_result
val to_source : t -> Async.Source.t