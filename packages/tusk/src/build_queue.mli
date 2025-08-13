(** Build queue management - Two-queue system for dependency ordering
    
    This module implements a two-queue system for managing package build order.
    Packages that are ready to build are in the current queue, while packages
    waiting on dependencies are in the later queue. *)

(** Abstract type representing a build queue *)
type t

(** {1 Queue Creation and Management} *)

(** Create a new build queue with a reference to build results.
    The queue will automatically skip packages that are already built
    and promote packages when their dependencies are ready. *)
val create : Build_results.t -> t

(** Clear both queues *)
val clear : t -> unit

(** {1 Adding Packages} *)

(** Add a package to the current (ready to build) queue *)
val add_ready : t -> string -> unit

(** Add a package to the later (waiting on dependencies) queue *)
val add_waiting : t -> string -> unit

(** {1 Queue Status} *)

(** Check if there's any work available in either queue *)
val is_empty : t -> bool

(** Check if the current queue has packages ready to build *)
val has_ready_work : t -> bool

(** Get queue statistics as (current_size, later_size) *)
val stats : t -> int * int

(** {1 Package Retrieval} *)

(** Get the next package from the current queue.
    Automatically skips packages that are already built and
    promotes packages from the waiting queue when their dependencies are ready.
    Returns [None] if no packages are available to build. *)
val take_ready : t -> string option

(** Peek at packages in the current queue without removing them *)
val peek_ready : t -> string list

(** Peek at packages in the later queue without removing them *)
val peek_waiting : t -> string list

(** {1 Queue Management} *)

(** Check if a package is in the waiting queue *)
val is_waiting : t -> string -> bool

(** Move a package from waiting to ready queue *)
val move_to_ready : t -> string -> unit


