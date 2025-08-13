(** Worker pool management for parallel builds
    
    This module manages a pool of worker processes for executing
    build tasks in parallel. The pool itself runs as a separate process
    that manages worker state and forwards completion messages. *)

open Miniriot 

(** Opaque type representing a running worker pool *)
type t

(** {1 Messages} *)

(** Messages sent from the worker pool to the listener *)
type Message.t +=
  | TaskAssigned of Build_messages.build_task
  (** Task was successfully assigned to a worker *)
  | NoWorkersAvailable of Build_messages.build_task  
  (** No workers available to handle the task *)
  | TaskCompleted of string * bool * Hasher.hash
  (** Task completed: (package_name, success, hash) *)

(** {1 Pool Management} *)

(** Start a worker pool process with the specified listener.
    The listener will receive TaskCompleted messages.
    Returns a handle to the pool. *)
val start : ?workers:int -> listener:Pid.t -> unit -> t

(** Send a task to the worker pool *)
val send_task : t -> Build_messages.build_task -> unit

(** Shutdown the worker pool *)
val shutdown : t -> unit