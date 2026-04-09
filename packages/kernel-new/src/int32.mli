type t = int32

(** Use `equal left right` for exact `Int32` equality. *)
val equal: t -> t -> bool

(** Use `compare left right` for `Int32` ordering. *)
val compare: t -> t -> int
