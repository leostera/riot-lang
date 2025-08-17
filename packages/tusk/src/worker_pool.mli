(** Worker pool management for parallel builds

    This module manages a pool of worker processes for executing build tasks in
    parallel. The pool itself runs as a separate process that manages worker
    state and forwards completion messages. *)

open Miniriot

type t
(** Opaque type representing a running worker pool *)

(** {1 Messages} *)

(** Messages sent from the worker pool to the listener *)
type Message.t +=
  | TaskAssigned of {
      task : Build_messages.build_task;
      worker_id : Worker_id.t;
    }
        (** Task was successfully assigned to a worker *)
  | NoWorkersAvailable of { task : Build_messages.build_task }
        (** No workers available to handle the task *)
  | TaskCompleted of {
      package_name : string;
      hash : Hasher.hash;
    }
        (** Task completed successfully *)
  | TaskFailed of {
      package_name : string;
      error : string;
    }
        (** Task failed with error message *)

(** {1 Pool Management} *)

val start : ?workers:int -> listener:Pid.t -> unit -> t
(** Start a worker pool process with the specified listener. The listener will
    receive TaskCompleted and TaskFailed messages. Returns a handle to the pool. *)

val send_task : t -> Build_messages.build_task -> unit
(** Send a task to the worker pool *)

val shutdown : t -> unit
(** Shutdown the worker pool *)
