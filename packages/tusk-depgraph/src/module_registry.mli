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

val create : unit -> t
(** Create a new empty registry *)

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
