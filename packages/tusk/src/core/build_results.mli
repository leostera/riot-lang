(** Build results tracking - Monitor and manage package build status

    This module tracks the build status of packages throughout the build
    process:

    A build_node is either:

    * pending -- it was registered as expected, and now we're waiting for it to
    be built * completed -- it was successfully registered and we have store
    artifacts to prove it * failed -- it was failed to complete and we have an
    error to show why

    A build_node can be moved from pending to complete or failed with
    mark_completed or mark_failed.

    A build_node can be marked as pending again to allow it to re-execute.

    We consider the build results to be all done once there are no targets
    marked as pending. *)

type t
(** Abstract type for build results tracker *)

(** {1 Creation and Management} *)

val create : unit -> t
(** Create a new build results tracker *)

val to_events : t -> Event.build_result list

(** {1 Package Initialization} *)

val all_done : t -> bool
(** Check if all tracked packages are done (built or failed) *)

val mark_pending : t -> Build_node.t -> unit
val mark_completed : t -> Build_node.t -> Artifact.t -> unit
val mark_failed : t -> Build_node.t -> error:string -> unit

(** {1 Status Queries} *)

type status =
  | NotStarted
  | Building
  | Built of Std.Crypto.hash
  | Failed of string

val get_status : t -> string -> status option
(** Get the build status of a package by name *)
