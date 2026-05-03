open Std

(** Opaque session identifiers. *)
type t

val make: unit -> t

val to_string: t -> string

val of_string: string -> t
