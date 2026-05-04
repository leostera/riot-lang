open Std

(** Opaque session identifiers. *)
type t

val make: unit -> t

val to_string: t -> string

val from_string: string -> t
