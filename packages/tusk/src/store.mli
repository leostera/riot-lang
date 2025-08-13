(** Content-addressable storage for build artifacts
    
    The store provides efficient caching of build outputs using
    content-based hashing, enabling incremental builds and artifact sharing. *)

(** Abstract type representing a store *)
type t

(** Artifact witness - proof that build outputs have been stored *)
type artifact = {
  hash : Hasher.hash;
  (** Content hash of the artifact *)
  
  files : string list;
  (** List of files included in this artifact *)
}

(** {1 Store Management} *)

(** Create a new store at the given root directory.
    The store will be located at [root_dir/target/debug/cache]. *)
val create : root_dir:string -> t

(** {1 Artifact Operations} *)

(** Check if artifacts for a given hash exist in the store *)
val exists : t -> Hasher.hash -> bool

(** List all artifact files for a given hash.
    Returns an empty list if the hash doesn't exist in the store. *)
val list_artifacts : t -> Hasher.hash -> string list

(** Store artifacts from sandbox to content-addressable store.
    [store_artifacts store hash sandbox_dir outputs] copies the specified
    output files from the sandbox directory to the store under the given hash.
    Returns an artifact witness proving the outputs have been stored. *)
val store_artifacts : t -> Hasher.hash -> string -> string list -> artifact

(** Promote artifacts from store to target directory.
    [promote_from_store store hash target_dir] copies all artifacts
    for the given hash to the target directory.
    Returns [true] if successful, [false] if hash doesn't exist. *)
val promote_from_store : t -> Hasher.hash -> string -> bool

(** {1 Store Maintenance} *)

(** Clean up old artifacts from the store.
    [gc_store store ~keep_recent_days] removes artifacts older than
    the specified number of days. (Not yet implemented) *)
val gc_store : t -> keep_recent_days:int -> unit

(** Get store statistics.
    Returns the total number of cached artifacts. *)
val get_stats : t -> int

(** {1 Internal} *)

(** Get the directory path for a given hash in the store.
    This is exposed for debugging purposes. *)
val get_hash_dir : t -> Hasher.hash -> string
