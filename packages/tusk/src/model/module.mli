(** OCaml module representation for the module graph *)

open Std

type t

val make : namespace:Namespace.t -> filename:Path.t -> t
(** Create a module from a namespace and filename *)

val module_name : t -> Module_name.t
(** Get the simple module name (e.g., "Path") *)

val namespaced_name : t -> string
(** Get the fully namespaced name (e.g., "Std__Path") *)

val qualified_name : t -> string
(** Alias for namespaced_name *)

val filename : t -> Path.t
(** Get the source file path *)

val kind : t -> [ `implementation | `interface ]
(** Get whether this is an implementation or interface file *)

val cmi : t -> Path.t
(** Get the compiled interface filename (e.g., "Std__Path.cmi") *)

val cmo : t -> Path.t
(** Get the compiled object filename (e.g., "Std__Path.cmo") *)

val eq : t -> t -> bool
(** Check if two modules are equal *)
