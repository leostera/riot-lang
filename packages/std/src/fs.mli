(** Filesystem utilities *)

type error = SystemError of string

val create_dir : Path.t -> (unit, error) Result.t
(** Create a directory if it doesn't exist *)

val file_exists : Path.t -> (bool, error) Result.t
(** Check if a file exists *)

val read_file : Path.t -> (string, error) Result.t
(** Read entire file contents *)

val write_file : Path.t -> string -> (unit, error) Result.t
(** Write string to file *)

val remove_file : Path.t -> (unit, error) Result.t
(** Remove a file *)

val is_directory : Path.t -> (bool, error) Result.t
(** Check if path is a directory *)

val is_regular_file : Path.t -> (bool, error) Result.t
(** Check if path is a regular file *)

val stat : Path.t -> (Unix.stats, error) Result.t
(** Get file statistics *)

val chmod : Path.t -> int -> (unit, error) Result.t
(** Change file permissions *)

val symlink : Path.t -> Path.t -> (unit, error) Result.t
(** Create a symbolic link from src to dst *)

val mkdir : Path.t -> int -> (unit, error) Result.t
(** Create a directory with permissions *)

val mkdir_safe : Path.t -> int -> (unit, error) Result.t
(** Create a directory if it doesn't exist, ignoring EEXIST errors *)

val mkdirp : Path.t -> (unit, error) Result.t
(** Create a directory and all parent directories *)

val rmdir : Path.t -> (unit, error) Result.t
(** Remove an empty directory *)

val remove_dir : Path.t -> (unit, error) Result.t
(** Remove a directory recursively *)

val opendir : Path.t -> (Unix.dir_handle, error) Result.t
(** Open a directory for reading *)

val readdir_handle : Unix.dir_handle -> (string, error) Result.t
(** Read next entry from directory handle *)

val closedir : Unix.dir_handle -> (unit, error) Result.t
(** Close a directory handle *)

val readdir : Path.t -> (string list, error) Result.t
(** Read all entries from a directory *)

val copy_file : Path.t -> Path.t -> (unit, error) Result.t
(** Copy a file from source to destination *)

val getcwd : unit -> (Path.t, error) Result.t
(** Get current working directory *)

val chdir : Path.t -> (unit, error) Result.t
(** Change current working directory *)
