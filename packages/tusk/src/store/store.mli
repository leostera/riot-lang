open Model

open Core
(** Content-addressable storage for build Artifact.ts *)

module Manifest = Manifest

type t
(** Abstract type representing a store *)

(** Artifact witness - proof that build outputs have been stored *)

type error = string

(** {1 Store Management} *)

val create : workspace:Workspace.t -> t
(** Create a new store for the given workspace *)

(** {1 Simple Interface} *)

val get : t -> Build_node.t -> Artifact.t option
(** Check if we have cached Artifact.ts for this build node. Returns Some
    Artifact.t if cached, None if not. *)

val save :
  t ->
  Build_node.t ->
  sandbox_dir:Std.Path.t ->
  outs:Std.Path.t list ->
  (Artifact.t, error) result
(** Save build outputs to the store. Copies the specified output files from
    sandbox_dir to the store. *)

(** {1 Artifact Operations} *)

val promote : t -> Artifact.t -> target_dir:Std.Path.t -> (unit, error) result
(** Promote cached Artifact.ts to the target directory *)

val exists : t -> Std.Crypto.hash -> bool
(** Check if Artifact.ts for a given hash exist in the store *)

val list_artifacts : t -> Std.Crypto.hash -> string list
(** List all files stored for a given hash *)

val promote_from_store : t -> Std.Crypto.hash -> Std.Path.t -> bool
(** Promote Artifact.ts directly from store by hash to target directory. Returns
    true if successful, false otherwise. *)

val get_hash_dir : t -> Std.Crypto.hash -> Std.Path.t
(** Get the directory path for a given hash in the store *)
