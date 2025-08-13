(** Build results tracking - Monitor and manage package build status
    
    This module tracks the build status of packages throughout the build
    process, including content-based hashing for incremental builds. *)

(** Build status for a package *)
type status = 
  | NotStarted 
  (** Package has not been built yet *)
  
  | Building 
  (** Package is currently being built *)
  
  | Built of Hasher.hash 
  (** Package built successfully with content hash *)
  
  | Failed of string 
  (** Package build failed with error message *)

(** Abstract type for build results tracker *)
type t

(** {1 Creation and Management} *)

(** Create a new build results tracker *)
val create : unit -> t

(** Clear all build results *)
val clear : t -> unit

(** {1 Package Initialization} *)

(** Initialize all packages as not started *)
val init_packages : t -> string list -> unit

(** Initialize a single package as not started *)
val init_package : t -> string -> unit

(** {1 Status Queries} *)

(** Check if a package is being tracked *)
val is_tracked : t -> string -> bool

(** Get the status of a package *)
val get_status : t -> string -> status option

(** Check if all dependencies are built *)
val dependencies_ready : t -> string list -> bool

(** Get list of unbuilt dependencies *)
val get_unbuilt_deps : t -> string list -> string list

(** Check if a package is currently being built *)
val is_building : t -> string -> bool

(** Check if all tracked packages are done (built or failed) *)
val all_done : t -> bool

(** {1 Status Updates} *)

(** Mark a package as building *)
val mark_building : t -> string -> unit

(** Mark a package as built with its content hash *)
val mark_built_with_hash : t -> string -> Hasher.hash -> unit

(** Mark a package as built (legacy - uses placeholder hash) *)
val mark_built : t -> string -> unit

(** Mark a package as failed with error message *)
val mark_failed : t -> string -> string -> unit

(** {1 Build Validation} *)

(** Check if a package is built with the current content hash *)
val is_built_with_current_hash : t -> string -> Hasher.hash -> bool

(** Check if build outputs exist for a package *)
val build_outputs_exist : Workspace.workspace -> string -> bool

(** Check if source files are newer than build outputs *)
val sources_newer_than_outputs : Workspace.workspace -> string -> bool

(** Check if a package is built and outputs are still valid (legacy timestamp-based) *)
val is_built_with_outputs_check : t -> string -> Workspace.workspace -> bool

(** {1 Statistics} *)

(** Get build statistics as (built, failed, building, not_started) *)
val get_stats : t -> int * int * int * int
