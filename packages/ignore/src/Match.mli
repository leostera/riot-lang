(** Outcome of evaluating ignore rules for one path. *)
type t =
  | Ignore
  | Whitelist
  | None_

(** Returns [true] when the path should be skipped. *)
val is_ignore: t -> bool

(** Returns [true] when the path was explicitly re-included. *)
val is_whitelist: t -> bool

(** Returns [true] when no rule matched. *)
val is_none: t -> bool

(** Left-biased combination helper. *)
val or_else: t -> t -> t
