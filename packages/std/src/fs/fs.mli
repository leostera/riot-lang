(** Filesystem utilities - PUBLIC INTERFACE *)

type error = SystemError of string

module Permissions : sig

  type t
  (** Unix file permissions *)

  val of_mode : int -> t
  (** Create from Unix mode bits *)

  val to_mode : t -> int
  (** Convert to Unix mode bits *)

  val readonly : t -> bool
  (** Check if no write bits are set *)

  val set_readonly : t -> bool -> t
  (** Set or clear all write permissions *)

  val user_read : t -> bool
  (** Check owner read permission *)

  val user_write : t -> bool
  (** Check owner write permission *)

  val user_execute : t -> bool
  (** Check owner execute permission *)

  val group_read : t -> bool
  (** Check group read permission *)

  val group_write : t -> bool
  (** Check group write permission *)

  val group_execute : t -> bool
  (** Check group execute permission *)

  val other_read : t -> bool
  (** Check others read permission *)

  val other_write : t -> bool
  (** Check others write permission *)

  val other_execute : t -> bool
  (** Check others execute permission *)

  val read_write : t
  (** rw-r--r-- (0644) - Owner read/write, group/others read-only *)

  val executable : t
  (** rwxr-xr-x (0755) - Owner read/write/execute, group/others read/execute *)

  val private_read_write : t
  (** rw------- (0600) - Owner read/write only, no access for others *)

  val private_executable : t
  (** rwx------ (0700) - Owner read/write/execute only, no access for others *)
end

module Metadata : sig

  type t
  (** File metadata *)

  val file_type :
    t ->
    [ `Regular
    | `Directory
    | `Symlink
    | `Block
    | `Character
    | `Fifo
    | `Socket ]
  (** Get the file type *)

  val is_file : t -> bool
  (** Check if it's a regular file *)

  val is_dir : t -> bool
  (** Check if it's a directory *)

  val is_symlink : t -> bool
  (** Check if it's a symbolic link *)

  val len : t -> int
  (** Get file size in bytes *)

  val permissions : t -> Permissions.t
  (** Get file permissions *)

  val accessed : t -> float
  (** Last access time *)

  val modified : t -> float
  (** Last modification time *)

  val created : t -> float option
  (** Creation time (platform-specific, may be None) *)

  val mode : t -> int
  (** Unix mode bits *)

  val uid : t -> int
  (** User ID of owner *)

  val gid : t -> int
  (** Group ID of owner *)

  val nlink : t -> int
  (** Number of hard links *)

  val ino : t -> int
  (** Inode number *)

  val dev : t -> int
  (** Device number *)

  val rdev : t -> int
  (** Device type (if special file) *)
end

module ReadDir : sig
  (** Directory iterator *)

  type t
  (** Opaque directory handle *)

  val next : t -> Path.t option
  (** Get next entry, or None when done. Skips . and .. *)

  val close : t -> (unit, error) Result.t
  (** Close the directory handle *)
end

module File : sig
  (** Handle-based file operations *)

  type t
  (** Opaque file handle *)

  val create : Path.t -> (t, error) result
  (** Create or truncate file for writing *)

  val create_new : Path.t -> (t, error) result
  (** Create new file, fail if exists *)

  val open_read : Path.t -> (t, error) result
  (** Open file for reading *)

  val open_write : Path.t -> (t, error) result
  (** Open file for writing, create if needed *)

  val open_append : Path.t -> (t, error) result
  (** Open file for appending *)

  val open_read_write : Path.t -> (t, error) result
  (** Open file for reading and writing *)

  val read : t -> bytes -> offset:int -> len:int -> (int, error) result
  (** Read up to len bytes, returns bytes actually read *)

  val read_to_end : t -> (string, error) result
  (** Read all remaining content as string *)

  val read_exact : t -> bytes -> offset:int -> len:int -> (unit, error) result
  (** Read exactly len bytes or fail *)

  val write : t -> bytes -> offset:int -> len:int -> (int, error) result
  (** Write bytes, returns bytes actually written *)

  val write_all : t -> string -> (unit, error) result
  (** Write entire string to file *)

  val write_string : t -> string -> (int, error) result
  (** Write string, returns bytes written *)

  val seek : t -> int64 -> (int64, error) result
  (** Seek to absolute position from start *)

  val seek_from_current : t -> int64 -> (int64, error) result
  (** Seek relative to current position *)

  val seek_from_end : t -> int64 -> (int64, error) result
  (** Seek relative to end of file *)

  val tell : t -> (int64, error) result
  (** Get current position in file *)

  val rewind : t -> (unit, error) result
  (** Seek to beginning of file *)

  val sync_all : t -> (unit, error) result
  (** Sync all data and metadata to disk *)

  val sync_data : t -> (unit, error) result
  (** Sync data only, not metadata *)

  val metadata : t -> (Metadata.t, error) result
  (** Get file metadata from handle *)

  val set_len : t -> len:int64 -> (unit, error) result
  (** Truncate or extend file to specified length *)

  val set_permissions : t -> permissions:Permissions.t -> (unit, error) result
  (** Change file permissions *)

  val lock_exclusive : t -> (unit, error) result
  (** Acquire exclusive lock, blocking *)

  val lock_shared : t -> (unit, error) result
  (** Acquire shared lock, blocking *)

  val try_lock_exclusive : t -> (bool, error) result
  (** Try to acquire exclusive lock, non-blocking *)

  val try_lock_shared : t -> (bool, error) result
  (** Try to acquire shared lock, non-blocking *)

  val unlock : t -> (unit, error) result
  (** Release lock *)

  val try_clone : t -> (t, error) result
  (** Duplicate file descriptor *)

  val close : t -> (unit, error) result
  (** Close file handle *)
end

(** {1 Path Operations} *)

val canonicalize : Path.t -> (Path.t, error) Result.t
(** Returns the canonical, absolute form of a path with all intermediate
    components normalized and symbolic links resolved. *)

val copy : src:Path.t -> dst:Path.t -> (unit, error) Result.t
(** Copies the contents of one file to another. This function will also copy the
    permission bits of the original file to the destination file. *)

val create_dir : Path.t -> (unit, error) Result.t
(** Create a single directory (non-recursive) *)

val create_dir_all : Path.t -> (unit, error) Result.t
(** Recursively create a directory and all of its parent components if they are
    missing. *)

val exists : Path.t -> (bool, error) Result.t
(** Returns Ok(true) if the path points at an existing entity. *)

val hard_link : src:Path.t -> dst:Path.t -> (unit, error) Result.t
(** Creates a new hard link on the filesystem. *)

val read : Path.t -> (string, error) Result.t
(** Read entire file as string *)

val read_dir : Path.t -> (Path.t MutIterator.t, error) Result.t
(** Returns an iterator over the entries within a directory. *)

val read_link : Path.t -> (Path.t, error) Result.t
(** Reads a symbolic link, returning the file that the link points to. *)

val read_to_string : Path.t -> (string, error) Result.t
(** Reads the entire contents of a file into a string (alias for read). *)

val remove_dir : Path.t -> (unit, error) Result.t
(** Remove empty directory *)

val remove_dir_all : Path.t -> (unit, error) Result.t
(** Removes a directory at this path, after removing all its contents. Use
    carefully! *)

val remove_file : Path.t -> (unit, error) Result.t
(** Remove a file *)

val rename : src:Path.t -> dst:Path.t -> (unit, error) Result.t
(** Renames a file or directory to a new name, replacing the original file if to
    already exists. *)

val set_permissions : Path.t -> Permissions.t -> (unit, error) Result.t
(** Changes the permissions found on a file or a directory. *)

val symlink : src:Path.t -> dst:Path.t -> (unit, error) Result.t
(** Create symbolic link *)

val write : string -> Path.t -> (unit, error) Result.t
(** Write string to file *)

(** {1 Metadata Queries} *)

val metadata : Path.t -> (Metadata.t, error) Result.t
(** Query file metadata, following symlinks (stat) *)

val symlink_metadata : Path.t -> (Metadata.t, error) Result.t
(** Query file metadata, NOT following symlinks (lstat) *)

(** {1 Convenience Queries} *)

val is_file : Path.t -> (bool, error) Result.t
(** Returns true if path is a regular file *)

val is_dir : Path.t -> (bool, error) Result.t
(** Returns true if path is a directory *)

(** {1 Utilities} *)

val with_tempdir : ?prefix:string -> (Path.t -> 'a) -> ('a, error) Result.t
(** Create a temporary directory, run a function with it, then clean it up. The
    temporary directory is automatically removed when the function returns, even
    if an exception is raised.
    @param prefix
      Optional prefix for the temporary directory name (default: "tmp")
    @param f Function to run with the temporary directory path
    @return Result of the function or an error if directory creation fails *)

