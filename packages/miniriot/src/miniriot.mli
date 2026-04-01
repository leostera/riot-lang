(** Miniriot - multicore actor runtime with per-worker queues, work stealing,
    and a dedicated reactor domain for timers and async I/O. *)
open Kernel

module Exception: sig
  (** Raised when a receive operation times out *)
  exception Receive_timeout
  (** Raised when a syscall operation times out *)
  exception Syscall_timeout
end
(** Runtime configuration *)
module Config = Config

module Runtime: sig
  (** Runtime support for reduction counting *)

  (** Reset the reduction count to a new value *)
  val reset_reductions: int -> unit
  (** Increment (actually decrement) the reduction count and yield if necessary.
      Due to how OCaml's bytecode works, we decrement from an initial value
      towards zero rather than counting up. The compiler-injected path shares
      the same process-local reduction budget as manual [yield] calls. *)
  val increment_reduction_count: unit -> unit
end

module Pid = Pid
(** Opaque worker/scheduler identifier type used by runtime internals. *)
module Scheduler_id = Scheduler_id

module Message: sig
  type t = ..
end

module Process: sig
  (** Process management *)
  type exit_reason = exn
  type flag =
    TrapExit of bool
  type monitor_ref
  type Message.t +=
    | EXIT of {
        from: Pid.t;
        reason: (unit, exit_reason) result;
      }
    | DOWN of {
        ref: monitor_ref;
        pid: Pid.t;
        reason: (unit, exit_reason) result;
      }

  type state =
    private | Uninitialized
    | Runnable
    | Waiting_message
    | Waiting_io of { name: string; token: Kernel.Async.Token.t; source: Kernel.Async.Source.t; }
    | Running
    | Exited of (unit, exit_reason) result
    | Finalized
  (** The process type *)
  type t
  (** Create a new process with the given function *)
  val make: (unit -> (unit, exit_reason) result) -> t
  (** Get the process ID *)
  val pid: t -> Pid.t
  (** Get the current state *)
  val state: t -> state
  (** Check if process is alive (not exited or finalized) *)
  val is_alive: t -> bool
  (** Check if process has messages in its mailbox *)
  val has_messages: t -> bool
  (** Send a message to the process from any scheduler domain. *)
  val send_message: t -> Message.t -> unit
  (** Mark process as waiting for I/O operation *)
  val mark_as_awaiting_io: t -> name:string -> Kernel.Async.Token.t -> Kernel.Async.Source.t -> unit
  (** Add a ready I/O token to the process *)
  val add_ready_token: t -> Kernel.Async.Token.t -> Kernel.Async.Source.t -> unit
  (** Get a ready I/O token if available *)
  val get_ready_token: t -> (Kernel.Async.Token.t * Kernel.Async.Source.t) option
  (** Consume all ready tokens with the given function *)
  val consume_ready_tokens: t -> (Kernel.Async.Token.t * Kernel.Async.Source.t -> unit) -> unit

  module Monitor: sig
    (** Monitor reference *)
    type t = monitor_ref
  end
  (** Link the current process to another process *)
  val link: Pid.t -> unit
  (** Unlink the current process from another process *)
  val unlink: Pid.t -> unit
  (** Monitor another process *)
  val monitor: Pid.t -> Monitor.t
  (** Stop monitoring a process *)
  val demonitor: Monitor.t -> unit
  (** Set flags for the currently running process. *)
  val set_flags: flag list -> unit
end
(** Opaque timer identifiers *)
module Timer_id = Timer_id

module Timer: sig
  (** Opaque timer identifier *)
  type id = Timer_id.t
  (** Send a message to a process after a delay (in seconds). Returns a timer ID
      that can be used to cancel the timer. *)
  val send_after: Pid.t -> Message.t -> after:float -> id
  (** Send a message to a process repeatedly at a given interval (in seconds).
      Returns a timer ID that can be used to cancel the timer. *)
  val send_interval: Pid.t -> Message.t -> interval:float -> id
  (** Cancel a timer by its ID. If the timer has already fired or doesn't exist,
      this is a no-op. *)
  val cancel: id -> unit
end

val self: unit -> Pid.t

val spawn: (unit -> (unit, Process.exit_reason) result) -> Pid.t

val spawn_link: (unit -> (unit, Process.exit_reason) result) -> Pid.t

val send: Pid.t -> Message.t -> unit
(** Spend one process-local cooperative reduction and yield to the scheduler
    when the budget is exhausted. *)
val yield: unit -> unit

type 'msg selector =
  Message.t -> [
    `select of 'msg
    | `skip
  ]
(** Receive a message using a selector. Optionally times out after [timeout]
    seconds, raising [Receive_timeout]. *)
val receive: selector:'value selector -> ?timeout:float -> unit -> 'value
(** Receive any message. Optionally times out after [timeout] seconds, raising
    [Receive_timeout]. *)
val receive_any: ?timeout:float -> unit -> Message.t

val shutdown: status:int -> unit

val syscall:
  ?timeout:float ->
  name:string ->
  interest:Kernel.Async.Interest.t ->
  source:Kernel.Async.Source.t ->
  (unit -> 'a) ->
  'a
(** Start the runtime with optional configuration. Defaults to millisecond timer
    resolution and [Config.default_scheduler_count] workers. *)
val run:
  main:(args:string list -> (unit, Process.exit_reason) result) ->
  args:string list ->
  ?config:Config.t ->
  unit ->
  unit
(** Enable debug tracing *)
val enable_trace: unit -> unit
(** Disable debug tracing *)
val disable_trace: unit -> unit

(** Snapshot of runtime multicore scheduler counters. *)
type trace_counters = {
  steals: int;
  failed_steals: int;
  remote_wakeups: int;
  duplicate_enqueue_races: int;
}
(** Return the current scheduler counter snapshot for the running runtime. *)
val trace_counters: unit -> trace_counters
(** Reset scheduler counters for the running runtime. *)
val reset_trace_counters: unit -> unit
