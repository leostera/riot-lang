(** System utilities - file operations, directory management, and process
    control

    This module provides a high-level interface to system operations,
    abstracting over Unix and Sys modules with error handling. *)

(** {1 File System Operations} *)

val file_exists : string -> bool
(** Check if a file or directory exists *)

val is_directory : string -> bool
(** Check if path is a directory *)

val is_regular_file : string -> bool
(** Check if a file is a regular file *)

val stat : string -> Unix.stats
(** Get file statistics *)

(** {1 Directory Operations} *)

val getcwd : unit -> string
(** Get current working directory *)

val chdir : string -> unit
(** Change current working directory *)

val mkdir : string -> int -> unit
(** Create a directory with permissions *)

val mkdir_safe : string -> int -> unit
(** Create a directory if it doesn't exist, ignoring EEXIST errors *)

val mkdirp : string -> unit
(** Create a directory and all parent directories *)

val rmdir : string -> unit
(** Remove a directory (must be empty) *)

val remove_dir : string -> unit
(** Remove a directory recursively *)

val list_dir_all : string -> string list
(** List all files in a directory *)

val list_dir : string -> (string -> bool) -> string list
(** List files in a directory with a filter function *)

(** {1 File Operations} *)

val remove_file : string -> unit
(** Remove a file *)

val copy_file : string -> string -> unit
(** Copy a file from source to destination *)

val read_file : string -> string
(** Read entire file as string *)

val write_file : string -> string -> unit
(** Write string to file *)

val chmod : string -> int -> unit
(** Make a file executable *)

val symlink : string -> string -> unit
(** Create a symbolic link *)

(** {1 Process and Command Execution} *)

val system : string -> Unix.process_status
(** Execute a shell command and return the exit status *)

val run_command : string -> bool * string
(** Run a shell command and return (success, output) *)

val run_process_lines : string -> string list
(** Run a command and capture its output as a list of lines *)

val exec : string -> string array -> 'a
(** Execute a program, replacing the current process *)

val getpid : unit -> int
(** Get process ID *)

val open_process_in : string -> in_channel
(** Open a process for reading *)

val close_process_in : in_channel -> Unix.process_status
(** Close a process opened for reading *)

val read_line : in_channel -> string
(** Read a line from an input channel *)

(** {1 System Information} *)

val os_type : unit -> string
(** Get OS type *)

val get_home : unit -> string
(** Get home directory *)

val argv : unit -> string array
(** Get command line arguments *)

val cpu_count : unit -> int
(** Get number of CPU cores *)

val time : unit -> float
(** Get current time as float *)

(** {1 Environment} *)

val putenv : string -> string -> unit
(** Set environment variable *)
