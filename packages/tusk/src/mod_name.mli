(** A module name, including namespace support *)

type namespace = string list
(** Type for module namespaces (list of capitalized strings) *)

type t = { filename : Std.Path.t; namespace : namespace; name : string }
(** Type representing a module with its filename, namespace, and name *)

val namespace_separator : string
(** Namespace separator used in qualified names *)

val namespace_of_string : string -> namespace
(** Namespace functions *)

val namespace_of_path : Std.Path.t -> namespace
val namespace_of_list : string list -> namespace
val namespace_append : namespace -> string -> namespace
val namespace_to_list : namespace -> string list

val make : filename:Std.Path.t -> namespace:namespace -> name:string -> t
(** ModName construction *)

val of_filename : ?namespace:namespace -> Std.Path.t -> t
val of_string : ?namespace:namespace -> string -> t

val filename : t -> Std.Path.t
(** Accessors *)

val module_name : t -> string
val namespace : t -> namespace
val qualified_name : t -> string

val cmo : t -> string
(** Output file names based on qualified names *)

val cmi : t -> string
val cmx : t -> string
val o : t -> string
val canonical_mli : t -> string
val canonical_ml : t -> string
