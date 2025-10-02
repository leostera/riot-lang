open Global
open Miniriot

type 'task worker = { pid : Pid.t; ref : 'task Ref.t }
(** Opaque worker handle - parameterized by task type for type safety *)

(** Messages from worker to coordinator *)
type worker_to_coordinator =
  | TaskCompleted of Pid.t  (** Worker finished a task and is ready for more *)

(** Messages from coordinator to worker *)
type coordinator_to_worker =
  | Task : 'task * 'task Ref.t -> coordinator_to_worker
      (** Assign a task to worker. Uses GADT + Ref.t for type safety *)
  | Stop  (** Shutdown the worker *)

(** Messages from owner to coordinator *)
type owner_to_coordinator =
  | SendTask : 'task * 'task Ref.t -> owner_to_coordinator
      (** Owner sends a task (in advanced mode) *)
  | SendTaskToWorker :
      'task worker * 'task * 'task Ref.t
      -> owner_to_coordinator
      (** Owner sends task to specific worker (after receiving WorkerReady) *)
  | Stop  (** Shutdown coordinator *)

(** Extend global Message.t with worker pool messages *)
type Message.t +=
  | WorkerReady : 'task worker -> Message.t
        (** Worker is ready - sent from coordinator to owner in advanced mode *)
  | ToCoordinator of owner_to_coordinator
        (** Message to coordinator from owner *)
  | ToWorker of coordinator_to_worker  (** Message to worker from coordinator *)
  | FromWorker of worker_to_coordinator
        (** Message to coordinator from worker *)
