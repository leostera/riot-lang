(** Worker pool management for parallel builds

    This module manages a pool of worker processes for executing build tasks in
    parallel. *)

open Miniriot
open Core
open Model

type t
(** Opaque type representing a running worker pool *)

(** {1 Pool Management} *)

val start :
  workers:int ->
  provider:Pid.t ->
  build_graph:Build_graph.t ->
  build_results:Build_results.t ->
  workspace:Workspace.t ->
  store:Store.t ->
  worker_fn:
    (Worker_pool_types.ctx -> unit -> (unit, Process.exit_reason) result) ->
  unit ->
  t
(** Start a worker pool with the specified number of workers. The provider will
    receive Worker messages from the pool. The worker_fn is the function to run
    for each worker with the shared context. *)

val send_task : Pid.t -> Worker_pool_types.task -> unit
(** Send a task to a specific worker *)
