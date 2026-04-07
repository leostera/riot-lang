(** Retention policy for a content store. *)
type t = {
  (** Maximum number of generations to keep, or [None] to keep all
      generations. *)
  keep_generations: int option;
  (** Maximum store size in bytes, or [None] for no size limit. *)
  max_size_bytes: int option;
}

(** Default content-store retention policy. *)
val default: t
