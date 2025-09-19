(** Module registry for managing module name resolution *)

type file_kind =
  | MLI  (** .mli interface file *)
  | ML  (** .ml implementation file *)
  | Alias  (** Generated alias module (e.g., Tusk__aliases.ml.gen) *)

type entry = {
  file : string;  (** Original file path, e.g., "build_node.ml" *)
  simple_name : string;  (** Simple module name, e.g., "Build_node" *)
  namespaced : string;
      (** Fully namespaced name, e.g., "Tusk__Core__Build_node" *)
  kind : file_kind;  (** What kind of file this is *)
  is_library_interface : bool;
      (** Whether this is a library interface module like core/core.ml *)
}

type t

val create : package_name:string -> t
(** Create a new empty registry for a package *)

val module_name_from_path : string -> string
(** Convert a file path to a module name, handling subdirectories *)

val make_namespaced : t -> string -> string
(** Create a namespaced module name from a module name *)

val entry_from_file : t -> string -> entry
(** Create a registry entry from a file path *)

val register : t -> entry -> unit
(** Register a module entry *)

val find_by_simple_name : t -> string -> entry list
(** Find all entries by simple module name (may return multiple for .ml/.mli) *)

val find_by_namespaced : t -> string -> entry list
(** Find all entries by namespaced name *)

val all_entries : t -> entry list
(** Get all registered entries *)

val dump : t -> unit
(** Print registry contents for debugging *)
