(** A module name with namespace support *)

open Std

type t
(** Abstract type representing a module name *)

val make : filename:Path.t -> namespace:Namespace.t -> name:string -> t
(** Create a module name with explicit components *)

val of_filename : ?namespace:Namespace.t -> Path.t -> t
(** Create from a filename, optionally with namespace *)

val of_string : ?namespace:Namespace.t -> string -> t
(** Create from a string name, optionally with namespace *)

val of_path : Path.t -> t
(** Create from a file path, extracting the module name *)

val filename : t -> Path.t
(** Get the original filename *)

val to_string : t -> string
(** Get the simple module name (without namespace) *)

val namespace : t -> Namespace.t
(** Get the namespace *)

val qualified_name : t -> string
(** Get the fully qualified name (namespace__name) *)

val cma : t -> Path.t
(** Get the .cmo filename based on qualified name *)

val cmo : t -> Path.t
(** Get the .cmo filename based on qualified name *)

val cmi : t -> Path.t
(** Get the .cmi filename based on qualified name *)

val cmx : t -> Path.t
(** Get the .cmx filename based on qualified name *)

val o : t -> Path.t
(** Get the .o filename based on qualified name *)

val canonical_mli : t -> Path.t
(** Get the canonical .mli filename *)

val canonical_ml : t -> Path.t
(** Get the canonical .ml filename *)
