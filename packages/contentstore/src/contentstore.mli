open Std

(** Generic content-addressable storage primitives. *)

(** Retention policy helpers. *)
module Policy = Policy

(** Namespace helpers for isolated store roots. *)
module Namespace = Namespace

(** Content-addressable store implementation. *)
module Store = Store

(** Content store handle. *)
type t = Store.t

(** Create one logical content store rooted at `root` scoped to `ns`. *)
val create: root:Path.t -> ns:Namespace.t -> policy:Policy.t -> t

(** Return the filesystem root for this store. *)
val root: t -> Path.t

(** Return the namespace used by this store. *)
val namespace: t -> Namespace.t

(** Return the retention policy used by this store. *)
val policy: t -> Policy.t

(** Return the hash-addressed directory for the given content hash. *)
val hash_dir_of: t -> Crypto.hash -> Path.t

(** Return `true` if the given hash already exists in the store. *)
val exists: t -> Crypto.hash -> bool

(** Atomically commit a staged directory as the given content hash. *)
val commit_dir: t -> hash:Crypto.hash -> source_dir:Path.t -> (unit, Store.error) result

(** Save a string payload as a hash-addressed object. *)
val save_object: t -> hash:Crypto.hash -> content:string -> (unit, Store.error) result

(** Save one filesystem file as a hash-addressed object. *)
val save_file: t -> hash:Crypto.hash -> source:Path.t -> (unit, Store.error) result

(** Open a hash-addressed object for reading. *)
val open_object: t -> hash:Crypto.hash -> (Fs.File.t, Store.error) result

(** Save a string payload under a stable namespace-local key. *)
val save_named_object: t -> key:string -> content:string -> (unit, Store.error) result

(** Save one filesystem file under a stable namespace-local key. *)
val save_named_file: t -> key:string -> source:Path.t -> (unit, Store.error) result

(** Open a namespace-local keyed object for reading. *)
val open_named_object: t -> key:string -> (Fs.File.t, Store.error) result
