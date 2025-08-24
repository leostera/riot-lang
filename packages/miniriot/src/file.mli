(** File system operations for Miniriot *)

type error =
  [ `File_not_found
  | `Permission_denied
  | `Is_a_directory
  | `Not_a_directory
  | `Already_exists
  | `No_space
  | `Unknown of string ]
(** File operation errors *)

val exists : path:string -> bool
(** Check if a file exists at the given path *)

val read : path:string -> (string, error) result
(** Read the entire contents of a file *)

val write : path:string -> content:string -> (unit, error) result
(** Write content to a file, creating or truncating as needed *)

val remove : path:string -> (unit, error) result
(** Remove a file *)

val list_dir : path:string -> (string list, error) result
(** List files and directories in a directory (excluding . and ..) *)

val list_dir_all : path:string -> (string list, error) result
(** List all files and directories in a directory (alias for list_dir) *)

val is_directory : path:string -> bool
(** Check if a path is a directory *)

val readdir : path:string -> (string list, error) result
(** Read directory contents (non-blocking) *)

val mkdir : path:string -> perm:int -> (unit, error) result
(** Create a directory with specified permissions *)

val mkdirp : path:string -> perm:int -> (unit, error) result
(** Create directory and all parent directories as needed *)

val copy_file : src:string -> dst:string -> (unit, error) result
(** Copy file from source to destination (non-blocking) *)

val file_exists : path:string -> (bool, error) result
(** Check if file exists (non-blocking) *)

val stat : path:string -> (Unix.stats, error) result
(** Get file statistics (non-blocking) *)

val chmod : path:string -> perm:int -> (unit, error) result
(** Change file permissions (non-blocking) *)

val symlink : src:string -> dst:string -> (unit, error) result
(** Create symbolic link (non-blocking) *)

val rmdir : path:string -> (unit, error) result
(** Remove empty directory (non-blocking) *)

val getcwd : unit -> (string, error) result
(** Get current working directory (non-blocking) *)

val chdir : path:string -> (unit, error) result
(** Change current working directory (non-blocking) *)

val opendir : path:string -> (Unix.dir_handle, error) result
(** Open directory for reading (non-blocking) *)

val readdir_handle : handle:Unix.dir_handle -> (string, error) result
(** Read next entry from directory handle (non-blocking) *)

val closedir : handle:Unix.dir_handle -> (unit, error) result
(** Close directory handle (non-blocking) *)
