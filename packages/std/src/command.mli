(** Command - OS process spawning and management

    This module provides a composable API for building and executing commands

*)

(** Process status types - for OS processes, not actors *)
type status = Exited of int | Signaled of int | Stopped of int

val of_unix_status : Unix.process_status -> status
(** Convert Unix process status to our type *)

(** Output from a command execution *)
type output = {
  status : int;  (** Exit status code *)
  stdout : string;  (** Standard output *)
  stderr : string;  (** Standard error *)
}

(** Builder for commands *)
type cmd

type error = 
  | SpawnFailed of string
  | CommandNotFound of string

(** {1 Command Building} *)

val make : string -> cmd
(** Create a new command. Example: [make "ls"] *)

val arg : string -> cmd -> cmd
(** Add a single argument. Example: [make "ls" |> arg "-l"] *)

val args : string list -> cmd -> cmd
(** Add multiple arguments. Example: [make "ls" |> args ["-l"; "-a"]] *)

val env : string -> string -> cmd -> cmd
(** Set an environment variable. Example: [make "ls" |> env "PATH" "/usr/bin"] *)

val envs : (string * string) list -> cmd -> cmd
(** Set multiple environment variables *)

val stdin : [ `Pipe | `Null | `Inherit ] -> cmd -> cmd
(** Configure stdin handling (default: Inherit) *)

val stdout : [ `Pipe | `Null | `Inherit ] -> cmd -> cmd
(** Configure stdout handling (default: Pipe for output, Inherit for status) *)

val stderr : [ `Pipe | `Null | `Inherit ] -> cmd -> cmd
(** Configure stderr handling (default: Pipe for output, Inherit for status) *)

(** {1 Command Execution} *)

val output : cmd -> (output, error) result
(** Execute command and capture output.
    Example: {[
      let cmd = Command.(make "ls" |> arg "-l") in
      match Command.output cmd with
      | Ok out ->
          Printf.printf "Exit: %d\nStdout:\n%s" out.status out.stdout
      | Error e -> Printf.printf "Failed: %s" (show_error e)
    ]} *)

val status : cmd -> (int, error) result
(** Execute command and return only the exit status.
    Stdout and stderr go to the parent's stdout/stderr.
    Example: {[
      let cmd = Command.(make "ls" |> arg "-l") in
      match Command.status cmd with
      | Ok 0 -> print_endline "Success!"
      | Ok n -> Printf.printf "Failed with code %d" n
      | Error e -> Printf.printf "Failed to run: %s" (show_error e)
    ]} *)

val show_error : error -> string
(** Convert error to string for display *)

(** {1 Legacy API} *)

type t
(** Opaque type representing a spawned command/process *)

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
val exec : ?args:string list -> string -> unit -> 'a

(** Get current process ID *)
val getpid : unit -> int

(** Execute a shell command and return the exit status *)
val system : string -> Unix.process_status

(** Open a process for reading *)
val open_process_in : string -> in_channel

(** Close a process opened for reading *)
val close_process_in : in_channel -> Unix.process_status

(** Run a shell command and return its output 
    @param env Optional environment variables to set as (key, value) pairs *)
val run_command : ?env:(string * string) list -> string -> (string, error) result

(** Run a command and capture its output as a list of lines *)
val run_process_lines : string -> string list

(** {1 Utilities} *)

(** FIXME: use Std.Env.current_executable : string instead *)
(** Get the name of the executable *)
val executable_name : string

(* FIXME: use Std.Env.args instead *)
(** Get command line arguments *)
val argv : unit -> string array
