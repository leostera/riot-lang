open Std

(** ## Types *)

type t
(** A table instance *)
type row = string list
(** A row is a list of cell values *)

(** ## Creation *)

val make : row list -> t
(** [make rows] creates a new table. *)
