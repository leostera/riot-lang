(** Build queue management - Three-state system for dependency ordering
    
    This module implements a three-state system for managing package build order.
    Packages can be ready to build, waiting on dependencies, or currently being built (busy).
    The queue automatically prevents duplicate task assignments and tracks busy state. *)

(** Abstract type representing a build queue *)
type t

(** {1 Queue Creation and Management} *)

(** Create a new build queue with a reference to build results.
    The queue will automatically skip packages that are already built
    and promote packages when their dependencies are ready. *)
val create : Build_results.t -> t

(** Clear all queues *)
val clear : t -> unit

(** {1 Adding Tasks} *)

(** Add a task to the ready (ready to build) queue *)
val add_ready : t -> Build_messages.build_task -> unit

(** Add a task to the waiting (waiting on dependencies) queue *)
val add_waiting : t -> Build_messages.build_task -> unit

(** {1 Queue Status} *)

(** Check if there's any work available in either queue *)
val is_empty : t -> bool

(** Check if the ready queue has tasks ready to build *)
val has_ready_work : t -> bool

(** Get queue statistics as (ready_size, waiting_size, busy_size) *)
val stats : t -> int * int * int

(** Check if a task is currently busy *)
val is_busy : t -> string -> bool

(** {1 Task Retrieval} *)

(** Get the next task from the ready queue and mark it as busy.
    Automatically skips tasks that are already built and
    promotes tasks from the waiting queue when their dependencies are ready.
    Returns [None] if no tasks are available to build. *)
val take_ready : t -> Build_messages.build_task option

(** Peek at package names in the ready queue without removing them *)
val peek_ready : t -> string list

(** Peek at package names in the waiting queue without removing them *)
val peek_waiting : t -> string list

(** {1 Task Management} *)

(** Check if a package is in the waiting queue *)
val is_waiting : t -> string -> bool

(** Move a task from waiting to ready queue *)
val move_to_ready : t -> string -> unit

(** Mark a task as completed and remove from busy queue *)
val mark_completed : t -> string -> unit
