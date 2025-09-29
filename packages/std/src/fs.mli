(** Filesystem utilities *)

type error = SystemError of string

(** Directory reading iterator *)
module ReadDir : sig
  type t

  val next : t -> Path.t option
  (** Get next entry from directory, skipping . and .. *)

  val close : t -> (unit, error) Result.t
  (** Close the directory handle *)
end

(** {1 Clean API - Preferred Functions} *)

val canonicalize : Path.t -> (Path.t, error) Result.t
(** Returns the canonical, absolute form of a path with all intermediate
    components normalized and symbolic links resolved. *)

val copy : src:Path.t -> dst:Path.t -> (unit, error) Result.t
(** Copies the contents of one file to another. This function will also copy the
    permission bits of the original file to the destination file. *)

val create_dir_all : Path.t -> (unit, error) Result.t
(** Recursively create a directory and all of its parent components if they are
    missing. *)

val exists : Path.t -> (bool, error) Result.t
(** Returns Ok(true) if the path points at an existing entity. *)

val hard_link : src:Path.t -> dst:Path.t -> (unit, error) Result.t
(** Creates a new hard link on the filesystem. *)

val metadata : Path.t -> (Unix.stats, error) Result.t
(** Given a path, queries the file system to get information about a file,
    directory, etc. *)

val read_dir : Path.t -> (Path.t MutIterator.t, error) Result.t
(** Returns an iterator over the entries within a directory. *)

val read_link : Path.t -> (Path.t, error) Result.t
(** Reads a symbolic link, returning the file that the link points to. *)

val read_to_string : Path.t -> (string, error) Result.t
(** Reads the entire contents of a file into a string. *)

val remove_dir_all : Path.t -> (unit, error) Result.t
(** Removes a directory at this path, after removing all its contents. Use
    carefully! *)

val rename : src:Path.t -> dst:Path.t -> (unit, error) Result.t
(** Renames a file or directory to a new name, replacing the original file if to
    already exists. *)

val set_permissions : Path.t -> int -> (unit, error) Result.t
(** Changes the permissions found on a file or a directory. *)

val write : string -> Path.t -> (unit, error) Result.t
(** Write string to file *)

(** {1 Legacy API - To be deprecated} *)

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

val with_tempdir : ?prefix:string -> (Path.t -> 'a) -> ('a, error) Result.t
(** Create a temporary directory, run a function with it, then clean it up. The
    temporary directory is automatically removed when the function returns, even
    if an exception is raised.
    @param prefix
      Optional prefix for the temporary directory name (default: "tmp")
    @param f Function to run with the temporary directory path
    @return Result of the function or an error if directory creation fails *)
