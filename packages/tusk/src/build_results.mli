(** Build results tracking - Monitor and manage package build status

    This module tracks the build status of packages throughout the build
    process, including content-based hashing for incremental builds. *)

(** Build status for a package *)
type status =
  | NotStarted  (** Package has not been built yet *)
  | Building  (** Package is currently being built *)
  | Built of Hasher.hash  (** Package built successfully with content hash *)
  | Failed of string  (** Package build failed with error message *)

type t
(** Abstract type for build results tracker *)

(** {1 Creation and Management} *)

val create : unit -> t
(** Create a new build results tracker *)

val clear : t -> unit
(** Clear all build results *)

(** {1 Package Initialization} *)

val init_packages : t -> string list -> unit
(** Initialize all packages as not started *)

val init_package : t -> string -> unit
(** Initialize a single package as not started *)

(** {1 Status Queries} *)

val is_tracked : t -> string -> bool
(** Check if a package is being tracked *)

val get_status : t -> string -> status option
(** Get the status of a package *)

val dependencies_ready : t -> string list -> bool
(** Check if all dependencies are built *)

val get_unbuilt_deps : t -> string list -> string list
(** Get list of unbuilt dependencies *)

val is_building : t -> string -> bool
(** Check if a package is currently being built *)

val all_done : t -> bool
(** Check if all tracked packages are done (built or failed) *)

(** {1 Status Updates} *)

val mark_building : t -> string -> unit
(** Mark a package as building *)

val mark_built_with_hash : t -> string -> Hasher.hash -> unit
(** Mark a package as built with its content hash *)

val mark_built : t -> string -> unit
(** Mark a package as built (legacy - uses placeholder hash) *)

val mark_failed : t -> string -> string -> unit
(** Mark a package as failed with error message *)

val reset_failed_packages : t -> unit
(** Reset all failed packages to NotStarted so they can be retried *)

(** {1 Build Validation} *)

val is_built_with_current_hash : t -> string -> Hasher.hash -> bool
(** Check if a package is built with the current content hash *)

val build_outputs_exist : Workspace.workspace -> string -> bool
(** Check if build outputs exist for a package *)

val sources_newer_than_outputs : Workspace.workspace -> string -> bool
(** Check if source files are newer than build outputs *)

val is_built_with_outputs_check : t -> string -> Workspace.workspace -> bool
(** Check if a package is built and outputs are still valid (legacy
    timestamp-based) *)

(** {1 Statistics} *)

val get_stats : t -> int * int * int * int
(** Get build statistics as (built, failed, building, not_started) *)
