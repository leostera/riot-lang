open Common

(** File handle for reading and writing *)

type t
(** Opaque file handle wrapping Kernel.Fd.t *)

(** {1 Opening Files} *)

val create : Path.t -> (t, error) result
(** Create or truncate file for writing (O_WRONLY | O_CREAT | O_TRUNC) *)

val create_new : Path.t -> (t, error) result
(** Create new file, fail if exists (O_WRONLY | O_CREAT | O_EXCL) *)

val open_read : Path.t -> (t, error) result
(** Open file for reading only (O_RDONLY) *)

val open_write : Path.t -> (t, error) result
(** Open file for writing, create if needed (O_WRONLY | O_CREAT) *)

val open_append : Path.t -> (t, error) result
(** Open file for appending (O_WRONLY | O_APPEND | O_CREAT) *)

val open_read_write : Path.t -> (t, error) result
(** Open file for reading and writing (O_RDWR) *)

(** {1 Reading} *)

val read : t -> bytes -> offset:int -> len:int -> (int, error) result
(** Read up to len bytes into buffer at offset. Returns bytes actually read.
    Uses async I/O with Miniriot syscalls. *)

val read_to_end : t -> (string, error) result
(** Read all remaining content as string *)

val read_exact : t -> bytes -> offset:int -> len:int -> (unit, error) result
(** Read exactly len bytes or fail *)

val read_line : t -> (string, error) result
(** Read a line from the file, including the newline character if present.
    Returns empty string on EOF. *)

(** {1 Writing} *)

val write : t -> bytes -> offset:int -> len:int -> (int, error) result
(** Write bytes from buffer at offset. Returns bytes actually written. Uses
    async I/O with Miniriot syscalls. *)

val write_all : t -> string -> (unit, error) result
(** Write entire string to file *)

val write_string : t -> string -> (int, error) result
(** Write string, returns bytes written *)

(** {1 Seeking} *)

val seek : t -> int64 -> (int64, error) result
(** Seek to absolute position from start of file *)

val seek_from_current : t -> int64 -> (int64, error) result
(** Seek relative to current position *)

val seek_from_end : t -> int64 -> (int64, error) result
(** Seek relative to end of file *)

val tell : t -> (int64, error) result
(** Get current position in file *)

val rewind : t -> (unit, error) result
(** Seek to beginning of file *)

(** {1 Synchronization} *)

val sync_all : t -> (unit, error) result
(** Sync all data and metadata to disk (fsync) *)

val sync_data : t -> (unit, error) result
(** Sync data only, not metadata (fdatasync on Linux, fsync elsewhere) *)

(** {1 Metadata & Properties} *)

val metadata : t -> (Metadata.t, error) result
(** Get file metadata from handle (fstat) *)

val set_len : t -> len:int64 -> (unit, error) result
(** Truncate or extend file to specified length (ftruncate) *)

val set_permissions : t -> permissions:Permissions.t -> (unit, error) result
(** Change file permissions (fchmod) *)

(** {1 File Locking} *)

val lock_exclusive : t -> (unit, error) result
(** Acquire exclusive lock, blocking (Unix.lockf F_LOCK) *)

val lock_shared : t -> (unit, error) result
(** Acquire shared lock, blocking (Unix.lockf F_RLOCK) *)

val try_lock_exclusive : t -> (bool, error) result
(** Try to acquire exclusive lock, non-blocking (Unix.lockf F_TLOCK) *)

val try_lock_shared : t -> (bool, error) result
(** Try to acquire shared lock, non-blocking (Unix.lockf F_TRLOCK) *)

val unlock : t -> (unit, error) result
(** Release lock (Unix.lockf F_ULOCK) *)

(** {1 Advanced} *)

val try_clone : t -> (t, error) result
(** Duplicate file descriptor (dup) *)

val into_fd : t -> Kernel.Fd.t
(** Extract raw file descriptor *)

val from_fd : Kernel.Fd.t -> t
(** Wrap file descriptor as file handle *)

(** {1 Closing} *)

val close : t -> (unit, error) result
(** Close file handle *)
