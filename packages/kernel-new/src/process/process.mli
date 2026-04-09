type t
type error =
  | File of Fs.File.error
  | Invalid_status of { tag: int }
  | System of System_error.t
val error_to_string: error -> string

type status =
  | Running
  | Exited of int
  | Signaled of int
  | Stopped of int
type input_stdio =
[
  `Null
  | `Pipe
  | `Inherit
  | `File of Fs.File.t
]
type output_stdio =
[
  `Null
  | `Pipe
  | `Inherit
  | `File of Fs.File.t
]
type error_stdio =
[
  `Null
  | `Pipe
  | `Inherit
  | `Redirect_to_stdout
  | `File of Fs.File.t
]
type stdio_config = {
  stdin: input_stdio;
  stdout: output_stdio;
  stderr: error_stdio;
}
val default_stdio: stdio_config

val spawn:
  program:string ->
  args:string array ->
  ?env:(string * string) array ->
  ?current_dir:Path.t ->
  stdio:stdio_config ->
  unit ->
  (t, error) Result.t

val pid: t -> int

val stdin: t -> Fs.File.t option

val stdout: t -> Fs.File.t option

val stderr: t -> Fs.File.t option

val try_wait: t -> (status option, error) Result.t

val to_source: t -> Async.Source.t

val kill: t -> signal:int -> (unit, error) Result.t

val close: t -> (unit, error) Result.t

val current_pid: unit -> int
