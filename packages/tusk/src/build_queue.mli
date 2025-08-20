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

(** {1 Adding Tasks} *)

val queue : t -> Build_node.t -> unit
(** Add task to the queue *)

val queue_with_deps : t -> Build_node.t -> deps:Build_node.t list -> unit
(** Add task to the queue but first add its dependencies to be built *)

(** {1 Task Retrieval} *)

val next_task : t -> Build_node.t option
(** Get the next task from the ready queue and mark it as busy. Automatically
    skips tasks that are already built and promotes tasks from the waiting queue
    when their dependencies are ready. Returns [None] if no tasks are available
    to build. *)
