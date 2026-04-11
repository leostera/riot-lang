open Std

(** Semantic identity for one binder visible to the type checker. *)
type t

(** Build one local binder identity. *)
val local: stamp:int -> name:string -> t

(** Build one prelude-owned binder identity. *)
val predef: stamp:int -> name:string -> t

(** Build one persistent binder identity from its exported surface path. *)
val persistent: SurfacePath.t -> t

(** Recover the printable binder name. *)
val name: t -> string

(** Recover the local/predef stamp when one exists. *)
val stamp: t -> int option

(** Structural equality over semantic binder identities. *)
val equal: t -> t -> bool

(** Total order over semantic binder identities. *)
val compare: t -> t -> int

(** Render a readable debug label. *)
val to_string: t -> string
