(** Content-addressable storage for build artifacts *)

type t
(** Abstract type representing a store *)

type artifact
(** Artifact witness - proof that build outputs have been stored *)

type error = string

(** {1 Store Management} *)

val create : root_dir:string -> t
(** Create a new store at the given root directory *)

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
