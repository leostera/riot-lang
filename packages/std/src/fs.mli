(** Filesystem utilities *)

(** FIXME: this module has a lot of shortnames/unix-like named functions like
    mkdirp that should be removed in favor of `create_dir_all` -- for ex: *
    chmod -> `set_permissions` * stat -> `metadata : Path.t -> (File.metadata,
    error) result * opendir/closedir : read_dir : Path.t -> (Std.Fs.ReadDir.t,
    error) result -- so we can use internal functions for iterating/closing it
    when we're done reading files, instead of asking the user to remember to
    closedir * getcwd -> `Std.Env.current_dir ()` already exists, so use that
    instead of having this function * chdir -> `Std.Env.set_current_dir : Path.t
    -> unit` should be used instaed

    basically we want these functions

    canonicalize : Path.t -> Path.t Returns the canonical, absolute form of a
    path with all intermediate components normalized and symbolic links
    resolved.

    copy : src:Path.t -> dst:Path.t -> (unit, error) result Copies the contents
    of one file to another. This function will also copy the permission bits of
    the original file to the destination file.

    create_dir : Path.t -> unit result Creates a new, empty directory at the
    provided path

    create_dir_all : Path.t -> unit result Recursively create a directory and
    all of its parent components if they are missing.

    exists : Path.t -> bool result Returns Ok(true) if the path points at an
    existing entity.

    hard_link : src:Path.t -> dst:Path.t -> unit result Creates a new hard link
    on the filesystem.

    metadata : Path.t -> File.metadata result Given a path, queries the file
    system to get information about a file, directory, etc.

    read : buf:Buffer.t -> Path.t -> int result Reads the entire contents of a
    file into a bytes vector.

    read_dir : Path.t -> ReadDir iterator result Returns an iterator over the
    entries within a directory.

    read_link : Path.t -> Path.t result Reads a symbolic link, returning the
    file that the link points to.

    read_to_string : Path.t -> string result Reads the entire contents of a file
    into a string.

    remove_dir : Path.t -> unit result Removes an empty directory.

    remove_dir_all : Path.t -> unit result Removes a directory at this path,
    after removing all its contents. Use carefully!

    remove_file : Path.t -> unit result Removes a file from the filesystem.

    rename : src:Path.t -> dst:PAth.t -> unit result Renames a file or directory
    to a new name, replacing the original file if to already exists.

    set_permissions : Path.t -> Std.Fs.permissions -> unit result Changes the
    permissions found on a file or a directory.

    write : string -> Path.t -> unit result *)

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
