open Std

(** Retention policy helpers. *)
module Policy = Policy

module Namespace = Namespace

(** Content-addressable store implementation. *)
module Store = Store

(** Content store handle. *)
type t = Store.t

(** Create one logical content store rooted at [root] scoped to [ns]. *)
val create: root:Path.t -> ns:Namespace.t -> policy:Policy.t -> t

val root: t -> Path.t

val namespace: t -> Namespace.t

val policy: t -> Policy.t

(** Return the hash-addressed directory for the given content hash. *)
val hash_dir_of: t -> Crypto.hash -> Path.t

(** Return `true` if the given hash already exists in the store. *)
val exists: t -> Crypto.hash -> bool

val commit_dir: t -> hash:Crypto.hash -> source_dir:Path.t -> (unit, Store.error) result

val save_object: t -> hash:Crypto.hash -> content:string -> (unit, Store.error) result

val save_file: t -> hash:Crypto.hash -> source:Path.t -> (unit, Store.error) result

val open_object: t -> hash:Crypto.hash -> (Fs.File.t, Store.error) result

val save_named_object: t -> key:string -> content:string -> (unit, Store.error) result

val save_named_file: t -> key:string -> source:Path.t -> (unit, Store.error) result

val open_named_object: t -> key:string -> (Fs.File.t, Store.error) result
