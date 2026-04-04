open Std

(** Host-supplied configuration for one [Session]. *)
type env = (string * TypeScheme.t) list
type t = {
  (** Primitive or host-supplied bindings visible in every source. *)
  prelude: env;
  (** Snapshot-scoped bindings synthesized from sibling sources or host context. *)
  ambient: env;
}

(** Default host configuration used by the current prototype and tests. *)
val default: t

(** Replace the snapshot ambient environment while preserving the base prelude. *)
val with_ambient: t -> ambient:env -> t
