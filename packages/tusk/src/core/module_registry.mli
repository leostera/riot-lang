(** Module registry for managing module name resolution.

    This module provides a centralized way to register and look up modules by
    their various names (simple, namespaced, file paths, etc.) *)

type entry = {
  file : string;  (** Original file path, e.g., "build_node.ml" *)
  simple_name : string;  (** Simple module name, e.g., "Build_node" *)
  namespaced : string;
      (** Fully namespaced name, e.g., "Tusk__Core__Build_node" *)
  submodule_names : string list;
      (** Alternative names, e.g., ["Core__Build_node"] *)
  is_alias : bool;  (** Whether this is an alias module *)
}

type t

val create : unit -> t
(** Create a new empty registry *)

val register : t -> entry -> unit
(** Register a module entry with all its name variations *)

val find_by_file : t -> string -> entry option
(** Find an entry by its file path *)

val find_by_simple_name : t -> string -> entry option
(** Find an entry by its simple module name *)

val find_by_namespaced : t -> string -> entry option
(** Find an entry by its fully namespaced name *)

val find_by_name : t -> string -> entry option
(** Find an entry by any of its names (simple, namespaced, or submodule) *)

val all_entries : t -> entry list
(** Get all registered entries *)

val entry_simple_name : entry -> string
(** Get the simple name of an entry *)

val entry_namespaced : entry -> string
(** Get the namespaced name of an entry *)

val entry_file : entry -> string
(** Get the file name of an entry *)

val entry_is_alias : entry -> bool
(** Check if entry is an alias module *)
