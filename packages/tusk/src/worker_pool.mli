(** Worker pool management for parallel builds

    This module manages a pool of worker processes for executing build tasks in
    parallel. *)

open Miniriot

type t
(** Opaque type representing a running worker pool *)

(** {1 Types} *)

type task = {
  node : Build_node.t;
  workspace : Workspace.t;
  session_id : Log.session_id option;
}
(** Build task with context *)

(** {1 Messages} *)

(** Worker pool messages *)
type Message.t +=
  | Worker of Message.t  (** Wrapper for worker messages *)
  | WorkerReady of Pid.t
  | Task of task
  | TaskCompleted of {
      worker : Pid.t;
      node : Build_node.t;
      artifact : Store.artifact;
    }
  | TaskFailed of { worker : Pid.t; node : Build_node.t; error : string }
  | RequeueWithDependencies of {
      worker : Pid.t;
      node : Build_node.t;
      deps : Build_node.t list;
    }

(** {1 Pool Management} *)

val start :
  workers:int ->
  provider:Pid.t ->
  worker_fn:(Pid.t -> Worker_id.t -> unit -> Process.exit_reason) ->
  unit ->
  t
(** Start a worker pool with the specified number of workers. The provider will
    receive Worker messages from the pool. The worker_fn is the function to run
    for each worker. *)

val send_task : Pid.t -> task -> unit
(** Send a task to a specific worker *)
