(** Reference type with unique identifiers *)

type 'a t

val make : unit -> 'a t
(** Create a new unique reference *)

val equal : 'a t -> 'b t -> bool
(** Check if two references are equal *)

val pp : Format.formatter -> 'a t -> unit
(** Pretty-print a reference *)

val type_equal : 'a t -> 'b t -> ('a, 'b) Type.eq option
(** Check if two references have the same type *)

val cast : 'a t -> 'b t -> 'a -> 'b option
(** Cast a value from one reference type to another if they are equal *)

val is_newer : 'a t -> 'b t -> bool
(** Check if the first reference was created after the second *)

val hash : 'a t -> int
(** Get hash value of a reference *)
