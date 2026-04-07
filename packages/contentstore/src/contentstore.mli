open Std

(** Retention policy helpers. *)
module Policy = Policy

(** Content-addressable store implementation. *)
module Store = Store

(** Content store handle. *)
type t = Store.t

(** Create a content store rooted at the given path. *)
val create: root:Path.t -> policy:Policy.t -> unit -> t

(** Return the root directory for the content store. *)
val root: t -> Path.t

(** Return the hash-addressed directory for the given content hash. *)
val hash_dir_of: t -> Crypto.hash -> Path.t

(** Return `true` if the given hash already exists in the store. *)
val exists: t -> Crypto.hash -> bool
