open Collections
open Sync
module Runtime_pid = Pid
module Runtime_process = Process
module Runtime_scheduler_id = Scheduler_id
module Runtime_timer = Timer
module Std_io = IO

type placement =
  | Normal
  | Pinned
  | Blocking

type blocking_lane = {
  lock: Sync.Mutex.t;
  cond: Sync.Condition.t;
  mutable domain: unit Kernel.Domain.t option;
}

type process_slot = {
  process: Runtime_process.t;
  (* Runtime-owned scheduling metadata.
     Process continuations/mailboxes live on [Process.t], while ownership and
     queue membership live here so workers can transfer slots without mutating
     process internals. *)
  placement: placement;
  owner_worker: Runtime_scheduler_id.t Sync.Atomic.t;
  mutable blocking_lane: blocking_lane option;
  queued: bool Sync.Atomic.t;
  (* A slot can be requested again while a worker is already stepping its
     continuation. Preserve that wakeup so it can be re-enqueued once the
     current step finishes instead of dropping or double-running the process. *)
  executing: bool Sync.Atomic.t;
  pending: bool Sync.Atomic.t;
}

type worker = {
  id: Runtime_scheduler_id.t;
  queue: process_slot Queue.t;
  lock: Sync.Mutex.t;
  cond: Sync.Condition.t;
}

type 'a response = {
  lock: Sync.Mutex.t;
  cond: Sync.Condition.t;
  mutable value: 'a option;
}

type reactor_command =
  | Add_timer of {
      now: int64;
      duration_nanos: int64;
      mode: Runtime_timer.mode;
      action: Runtime_timer.action;
      reply: Runtime_timer.id response
    }
  | Cancel_timer of Runtime_timer.id
  | Register_io of {
      token: Kernel.Async.Token.t;
      interest: Kernel.Async.Interest.t;
      source: Kernel.Async.Source.t;
      reply: (unit, Std_io.error) Kernel.result response
    }
  | Deregister_io of Kernel.Async.Source.t

type process_shard = {
  lock: Sync.Mutex.t;
  processes: (Runtime_pid.t, process_slot) HashMap.t;
}

type process_registry = {
  shards: process_shard array;
  size: int Sync.Atomic.t;
}

type runtime_counters = {
  steals: int Sync.Atomic.t;
  failed_steals: int Sync.Atomic.t;
  remote_wakeups: int Sync.Atomic.t;
  duplicate_enqueue_races: int Sync.Atomic.t;
}

type t = {
  stop: bool Sync.Atomic.t;
  status: int Sync.Atomic.t;
  workers: worker array;
  processes: process_registry;
  counters: runtime_counters;
  relations_lock: Sync.Mutex.t;
  reactor_commands: reactor_command Queue.t;
  reactor_lock: Sync.Mutex.t;
  io_poll: Kernel.Async.Poll.t;
  timer_wheel: Timer_wheel.t;
  blocking_lanes_lock: Sync.Mutex.t;
  mutable blocking_lanes: blocking_lane list;
  config: Config.t;
}

type domain_context = {
  scheduler: t;
  worker_id: Runtime_scheduler_id.t option;
  mutable current_process: Runtime_process.t option;
}

let current_context: domain_context option Kernel.Domain.DLS.key =
  Kernel.Domain.DLS.new_key (fun () -> None)
