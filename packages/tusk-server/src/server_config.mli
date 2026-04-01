open Std

(** Server configuration *)
type t
(** Default configuration with all features enabled *)
val default: t
(** Check if two configurations are equal *)
val equal: t -> t -> bool
