(** Command - OS process spawning and management

    This module provides a composable API for building and executing commands as
    OS processes. *)

type status = int
type output = { stdout : string; stderr : string; status : status }
type t
type error = SystemError of string

val make :
  ?cwd:Path.t -> ?env:(string * string) list -> ?args:string list -> string -> t
(* Create a new command *)

val output : t -> (output, error) result
(* Execute command as a child process, and collect its stdout and stderr *)

val status : t -> (status, error) result
(* Execute command as a child process, not collecting its stdout/stderr, and returns the status code *)
