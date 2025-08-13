(** System utilities - file operations, directory management, and process control
    
    This module provides a high-level interface to system operations,
    abstracting over Unix and Sys modules with error handling. *)

(** {1 File System Operations} *)

(** Check if a file or directory exists *)
val file_exists : string -> bool

(** Check if path is a directory *)
val is_directory : string -> bool

(** Check if a file is a regular file *)
val is_regular_file : string -> bool

(** Get file statistics *)
val stat : string -> Unix.stats

(** {1 Directory Operations} *)

(** Get current working directory *)
val getcwd : unit -> string

(** Change current working directory *)
val chdir : string -> unit

(** Create a directory with permissions *)
val mkdir : string -> int -> unit

(** Create a directory if it doesn't exist, ignoring EEXIST errors *)
val mkdir_safe : string -> int -> unit

(** Create a directory and all parent directories *)
val mkdirp : string -> unit

(** Remove a directory (must be empty) *)
val rmdir : string -> unit

(** Remove a directory recursively *)
val remove_dir : string -> unit

(** List all files in a directory *)
val list_dir_all : string -> string list

(** List files in a directory with a filter function *)
val list_dir : string -> (string -> bool) -> string list

(** {1 File Operations} *)

(** Remove a file *)
val remove_file : string -> unit

(** Copy a file from source to destination *)
val copy_file : string -> string -> unit

(** Read entire file as string *)
val read_file : string -> string

(** Write string to file *)
val write_file : string -> string -> unit

(** Make a file executable *)
val chmod : string -> int -> unit

(** Create a symbolic link *)
val symlink : string -> string -> unit

(** {1 Process and Command Execution} *)

(** Execute a shell command and return the exit status *)
val system : string -> Unix.process_status

(** Run a shell command and return (success, output) *)
val run_command : string -> bool * string

(** Run a command and capture its output as a list of lines *)
val run_process_lines : string -> string list

(** Execute a program, replacing the current process *)
val exec : string -> string array -> 'a

(** Get process ID *)
val getpid : unit -> int

(** Open a process for reading *)
val open_process_in : string -> in_channel

(** Close a process opened for reading *)
val close_process_in : in_channel -> Unix.process_status

(** {1 System Information} *)

(** Get OS type *)
val os_type : unit -> string

(** Get home directory *)
val get_home : unit -> string

(** Get command line arguments *)
val argv : unit -> string array

(** Get number of CPU cores *)
val cpu_count : unit -> int

(** Get current time as float *)
val time : unit -> float

(** {1 Environment} *)

(** Set environment variable *)
val putenv : string -> string -> unit