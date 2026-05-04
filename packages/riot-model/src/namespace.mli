(** Module namespace handling with double-underscore convention *)

(** Abstract type for namespaces *)

(** Empty namespace *)
type t

val empty: t

(** Create namespace from string (splits on __) *)
val from_string: string -> t

(** Create namespace from list of components *)
val from_list: string list -> t

(** Append a component to namespace *)
val append: t -> string -> t

(** Convert to string with __ separator *)
val to_string: t -> string

(** Get list of namespace components *)
val to_list: t -> string list

(** Check if namespace is empty *)
val is_empty: t -> bool
