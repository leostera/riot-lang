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

val clear : t -> unit
(** Clear all queues *)

(** {1 Adding Tasks} *)

val add_ready : t -> Build_messages.build_task -> unit
(** Add a task to the ready (ready to build) queue *)

val add_waiting : t -> Build_messages.build_task -> unit
(** Add a task to the waiting (waiting on dependencies) queue *)

(** {1 Queue Status} *)

val is_empty : t -> bool
(** Check if there's any work available in either queue *)

val has_ready_work : t -> bool
(** Check if the ready queue has tasks ready to build *)

val stats : t -> int * int * int
(** Get queue statistics as (ready_size, waiting_size, busy_size) *)

val is_busy : t -> string -> bool
(** Check if a task is currently busy *)

(** {1 Task Retrieval} *)

val take_ready : t -> Build_messages.build_task option
(** Get the next task from the ready queue and mark it as busy. Automatically
    skips tasks that are already built and promotes tasks from the waiting queue
    when their dependencies are ready. Returns [None] if no tasks are available
    to build. *)

val peek_ready : t -> string list
(** Peek at package names in the ready queue without removing them *)

val peek_waiting : t -> string list
(** Peek at package names in the waiting queue without removing them *)

(** {1 Task Management} *)

val is_waiting : t -> string -> bool
(** Check if a package is in the waiting queue *)

val move_to_ready : t -> string -> unit
(** Move a task from waiting to ready queue *)

val mark_completed : t -> string -> unit
(** Mark a task as completed and remove from busy queue *)
