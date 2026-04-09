type t = float

(** Use `equal left right` for the raw runtime float equality semantics. *)
val equal: t -> t -> bool

(** Use `compare left right` for the runtime float ordering semantics. *)
val compare: t -> t -> int
