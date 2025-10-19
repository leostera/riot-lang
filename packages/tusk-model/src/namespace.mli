(** Module namespace handling with double-underscore convention *)

type t
(** Abstract type for namespaces *)

val empty : t
(** Empty namespace *)

val of_string : string -> t
(** Create namespace from string (splits on __) *)

val of_list : string list -> t
(** Create namespace from list of components *)

val append : t -> string -> t
(** Append a component to namespace *)

val to_string : t -> string
(** Convert to string with __ separator *)

val to_list : t -> string list
(** Get list of namespace components *)

val is_empty : t -> bool
(** Check if namespace is empty *)
