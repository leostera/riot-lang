(**
   Operating system process primitives.

   Use `Process` for spawned operating system processes such as child
   commands. For actor runtime lifecycle and monitoring, use `Actor`.
*)
open Kernel

type t
type error =
  | File of Kernel.Fs.File.error
  | InvalidStatus of { tag: int }
  | System of Kernel.SystemError.t

val error_to_string: error -> string

type status =
  | Running
  | Exited of int
  | Signaled of int
  | Stopped of int

module Stdin: sig
  type t =
    | Null
    | Pipe
    | Inherit
    | File of Kernel.Fs.File.t
end

module Stdout: sig
  type t =
    | Null
    | Pipe
    | Inherit
    | File of Kernel.Fs.File.t
end

module Stderr: sig
  type t =
    | Null
    | Pipe
    | Inherit
    | RedirectToStdout
    | File of Kernel.Fs.File.t
end

type input_stdio = Stdin.t
type output_stdio = Stdout.t
type error_stdio = Stderr.t
type stdio_config = {
  stdin: input_stdio;
  stdout: output_stdio;
  stderr: error_stdio;
}

(** Default stdio inherits the parent process streams. *)
val default_stdio: stdio_config

(**
   Use `spawn ...` for immediate process creation.

   Waiting for exit stays separate through `try_wait` and `to_source`.
*)
val spawn:
  program:string ->
  args:string array ->
  ?env:(string * string) array ->
  ?current_dir:Kernel.Path.t ->
  stdio:stdio_config ->
  unit ->
  (t, error) Result.t

val pid: t -> int

(**
   Pipe handles are present only when the corresponding stdio mode requested `Stdin.Pipe`,
   `Stdout.Pipe`, or `Stderr.Pipe`.
*)
val stdin: t -> Kernel.Fs.File.t option

val stdout: t -> Kernel.Fs.File.t option

val stderr: t -> Kernel.Fs.File.t option

(**
   Use `try_wait process` to observe exit state without blocking.

   Final `Exited _` and `Signaled _` results stay stable once observed. `Stopped _` is transient:
   a later `SIGCONT` plus final exit may replace it on subsequent calls.
*)
val try_wait: t -> (status option, error) Result.t

(**
   Use `to_source process` when you want readiness for `try_wait`.

   Exit observation remains valid even if owned stdio handles are closed before the process is
   reaped.
*)
val to_source: t -> Kernel.Async.Source.t

(** Use `kill process ~signal` to send a signal immediately without waiting for exit. *)
val kill: t -> signal:int -> (unit, error) Result.t

(**
   Use `execv program argv` to replace the current process image immediately.

   On success it does not return. On failure it returns the underlying system error.
*)
val execv: program:string -> args:string array -> (unit, Kernel.SystemError.t) Result.t

(**
   Use `close process` to close owned pipe handles without discarding exit observation through
   `try_wait` or `to_source`.
*)
val close: t -> (unit, error) Result.t

(** Use `id ()` to read the current operating system process identifier immediately. *)
val id: unit -> int32
