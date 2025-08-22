(** Command - OS process spawning and management *)

type t
(** Opaque type representing a spawned command/process *)

type error = 
  | SpawnFailed of string
  | CommandNotFound of string

(** Spawn a new OS process *)
val spawn : cmd:string -> args:string list -> (t, error) result

(** Get the OS process ID *)
val pid : t -> int

(** Check if a process is running *)
val is_running : t -> bool

(** Kill a process *)
val kill : t -> (unit, error) result

(** Check if a process ID is running *)
val is_pid_running : int -> bool

(** Execute a program, replacing the current process *)
val exec : string -> string array -> 'a

(** Get current process ID *)
val getpid : unit -> int

(** Execute a shell command and return the exit status *)
val system : string -> Unix.process_status

(** Open a process for reading *)
val open_process_in : string -> in_channel

(** Close a process opened for reading *)
val close_process_in : in_channel -> Unix.process_status

(** Run a shell command and return its output *)
val run_command : string -> (string, error) result

(** Run a command and capture its output as a list of lines *)
val run_process_lines : string -> string list

(** FIXME: use Std.Env.current_executable : string instead *)
(** Get the name of the executable *)
val executable_name : string

(* FIXME: use Std.Env.args instead *)
(** Get command line arguments *)
val argv : unit -> string array
