type t

type error =
  | File of Fs.File.error
  | InvalidStatus of { tag: int }
  | System of System_error.t

val error_to_string: error -> string

type status =
  | Running
  | Exited of int
  | Signaled of int
  | Stopped of int

module Stdin : sig
  type t =
    | Null
    | Pipe
    | Inherit
    | File of Fs.File.t
end

module Stdout : sig
  type t =
    | Null
    | Pipe
    | Inherit
    | File of Fs.File.t
end

module Stderr : sig
  type t =
    | Null
    | Pipe
    | Inherit
    | RedirectToStdout
    | File of Fs.File.t
end

type input_stdio = Stdin.t

type output_stdio = Stdout.t

type error_stdio = Stderr.t

type stdio_config = { stdin: input_stdio; stdout: output_stdio; stderr: error_stdio }

val default_stdio: stdio_config

val spawn: program:string -> args:string array -> ?env:(string * string) array -> ?current_dir:Path.t -> stdio:stdio_config -> unit -> (t, error) Result.t

val pid: t -> int

val stdin: t -> Fs.File.t option

val stdout: t -> Fs.File.t option

val stderr: t -> Fs.File.t option

val try_wait: t -> (status option, error) Result.t

val to_source: t -> Async.Source.t

val kill: t -> signal:int -> (unit, error) Result.t

val execv: string -> string array -> (unit, System_error.t) Result.t

val close: t -> (unit, error) Result.t

val current_pid: unit -> int
