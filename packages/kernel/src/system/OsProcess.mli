open Global0

type t
(** A running OS process handle *)

type status =
  | Running
  | Exited of int
  | Signaled of int
  | Stopped of int  (** Process status *)

type stdio_config = {
  stdin : [ `Null | `Pipe | `Inherit | `File of Fd.t ];
  stdout : [ `Null | `Pipe | `Inherit | `File of Fd.t ];
  stderr : [ `Null | `Pipe | `Inherit | `Redirect_to_stdout | `File of Fd.t ];
}
(** Standard I/O configuration for spawned process.
    - `Null: redirect to /dev/null
    - `Pipe: create a pipe for parent-child communication
    - `Inherit: inherit from parent process
    - `File fd: redirect to an open file descriptor
    - `Redirect_to_stdout: (stderr only) redirect stderr to stdout *)

val spawn :
  program:string ->
  args:string list ->
  ?env:(string * string) list ->
  ?cwd:string ->
  stdio:stdio_config ->
  unit ->
  (t, [> `SpawnFailed of string ]) result
(** Spawn a process. All piped file descriptors are set to non-blocking mode.
    Returns a process handle that can be used for I/O and status checking. *)

val stdin : t -> Fd.t option
(** Get stdin fd if configured as `Pipe. Ready for non-blocking writes. *)

val stdout : t -> Fd.t option
(** Get stdout fd if configured as `Pipe. Ready for non-blocking reads. *)

val stderr : t -> Fd.t option
(** Get stderr fd if configured as `Pipe. Ready for non-blocking reads. *)

val pid : t -> int
(** Get the OS process ID *)

val try_wait : t -> status option
(** Non-blocking status check using WNOHANG. Returns Some status if process has
    changed state, None if still running. This is the only way to check process
    status at the Kernel level - higher layers can implement async waiting by
    polling this with yields. *)

val kill : t -> signal:int -> unit
(** Send a signal to the process *)

val close : t -> unit
(** Close all file descriptors and release resources. Does not wait for process
    termination - call try_wait first if you need the exit status. *)

val current_pid : unit -> int
(** Get the current process ID *)
