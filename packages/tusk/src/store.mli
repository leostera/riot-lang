(** Content-addressable storage for build artifacts *)

type t
(** Abstract type representing a store *)

type artifact = { hash : Hasher.hash; files : string list }
(** Artifact witness - proof that build outputs have been stored *)

type error = string

(** {1 Store Management} *)

val create : workspace:Workspace.t -> t
(** Create a new store for the given workspace *)

(** {1 Simple Interface} *)

val get : t -> Build_node.t -> artifact option
(** Check if we have cached artifacts for this build node. Returns Some artifact
    if cached, None if not. *)

val save :
  t ->
  Build_node.t ->
  sandbox_dir:string ->
  outs:Std.Path.t list ->
  (artifact, error) result
(** Save build outputs to the store. Copies the specified output files from
    sandbox_dir to the store. *)

(** {1 Artifact Operations} *)

val promote : t -> artifact -> target_dir:string -> (unit, error) result
(** Promote cached artifacts to the target directory *)

val exists : t -> Hasher.hash -> bool
(** Check if artifacts for a given hash exist in the store *)

val list_artifacts : t -> Hasher.hash -> string list
(** List all files stored for a given hash *)

val promote_from_store : t -> Hasher.hash -> string -> bool
(** Promote artifacts directly from store by hash to target directory. Returns
    true if successful, false otherwise. *)

val get_hash_dir : t -> Hasher.hash -> string
(** Get the directory path for a given hash in the store *)
