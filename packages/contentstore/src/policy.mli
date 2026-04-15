(** Placeholder policy handle for contentstore.

    Retention and pruning policy are intentionally not modeled yet because the
    current store only provides immutable object storage and mutable named
    values without generation tracking. *)
type t
val default: t
