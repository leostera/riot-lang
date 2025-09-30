(** Build queue management - Three-state system for dependency ordering

    This module implements a three-state system for managing package build
    order. Packages can be ready to build, waiting on dependencies, or currently
    being built (busy). The queue automatically prevents duplicate task
    assignments and tracks busy state. *)

type t
(** Abstract type representing a build queue *)

(** {1 Queue Creation and Management} *)

val create : Build_results.t -> t
(** Create a new build queue with a reference to build results. The queue will
    automatically skip packages that are already built and promote packages when
    their dependencies are ready. *)

(** {1 Core Operations} *)

val queue : t -> Build_node.t -> unit
(** Add task to ready queue if not already busy/built/queued *)

val next : t -> Build_node.t option
(** Get next ready task, swap queues if needed. Marks task as busy. *)

val requeue_with_deps : t -> Build_node.t -> deps:Build_node.t list -> unit
(** Requeue task to later queue and queue all dependencies. Removes task from
    busy set. *)

val mark_as_completed : t -> Build_node.t -> artifact:Artifact.t -> unit
(** Mark task as completed - removes from busy and updates build results *)

val mark_as_failed : t -> Build_node.t -> error:string -> unit
(** Mark task as failed - removes from busy and updates build results *)

val get_stats : t -> int * int * int
(** Get queue statistics as (ready_count, waiting_count, busy_count) *)
