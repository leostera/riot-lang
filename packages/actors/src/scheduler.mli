(** Multicore scheduler/runtime entrypoint for Actors.

    The runtime owns:
    - a process registry shared by all workers
    - one runnable queue per scheduler worker
    - a dedicated reactor domain that owns timers and async I/O polling
*)
open Kernel

type t
(** Opaque scheduler runtime. *)
type trace_counters = {
  steals: int;
  failed_steals: int;
  remote_wakeups: int;
  duplicate_enqueue_races: int;
}

(** Snapshot of runtime scheduler counters used for multicore tracing. *)
val get_scheduler: unit -> t

(** Return the scheduler runtime bound to the current domain context. *)
val spawn: t -> (unit -> (unit, Process.exit_reason) result) -> Pid.t

(** Spawn a process on a scheduler chosen by runtime placement policy. *)
val spawn_pinned:
  ?worker_id:Scheduler_id.t -> t -> (unit -> (unit, Process.exit_reason) result) -> Pid.t

(** Spawn a process pinned to a single normal scheduler. *)
val spawn_blocked: t -> (unit -> (unit, Process.exit_reason) result) -> Pid.t

(** Spawn a process on a dedicated blocking lane outside the normal
    work-stealing scheduler pool. *)
val self: unit -> Pid.t

(** Return the PID of the currently running process in this domain context. *)
val current_worker_id_opt: unit -> Scheduler_id.t option

(** Return the current normal scheduler identifier, or [None] when not running
    on a normal scheduler worker. *)
val send: Pid.t -> Message.t -> unit

(** Send a message by PID through the runtime-wide process registry. *)
val kill: t -> Pid.t -> Process.exit_reason -> unit

(** Request that a process exit at its next scheduler boundary. *)
val shutdown: t -> status:int -> unit

(** Request runtime-wide shutdown with an exit status. *)
val run: config:Config.t -> main:(unit -> (unit, Process.exit_reason) result) -> int

(** Start worker domains + reactor and run until shutdown. *)
val add_timer:
  t -> now:int64 -> duration_nanos:int64 -> mode:Timer.mode -> action:Timer.action -> Timer.id

(** Register a timer in the reactor-owned timer wheel. *)
val cancel_timer: t -> Timer.id -> unit

(** Cancel a timer in the reactor-owned timer wheel. *)
val get_current_process: unit -> Process.t

(** Return the current process for the calling worker domain context. *)
val get_process: t -> Pid.t -> Process.t option

(** Runtime-wide process lookup by PID. *)
val with_relations_lock: t -> (unit -> 'a) -> 'a

(** Serialize link/monitor relation updates that span multiple processes. *)
val enable_trace: unit -> unit

(** Enable scheduler trace logging and runtime counter emission. *)
val disable_trace: unit -> unit

(** Disable scheduler trace logging. *)
val trace_counters: t -> trace_counters

(** Return a point-in-time snapshot of scheduler counters for this runtime. *)
val reset_trace_counters: t -> unit

(** Reset scheduler counters to zero for this runtime. *)
