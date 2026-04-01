(** OCaml module representation for the module graph *)
open Std

(** Create a module from a namespace and filename *)
type t
val make: namespace:Namespace.t -> filename:Path.t -> t
(** Get the simple module name (e.g., "Path") *)
val module_name: t -> Module_name.t
(** Get the fully namespaced name (e.g., "Std__Path") *)
val namespaced_name: t -> string
(** Alias for namespaced_name *)
val qualified_name: t -> string
(** Get the source file path *)
val filename: t -> Path.t
(** Get whether this is an implementation or interface file *)

(** Get the compiled interface filename (e.g., "Std__Path.cmi") *)
val kind: t -> [
    | `implementation
    | `interface
  ]

val cmi: t -> Path.t
(** Get the compiled object filename (e.g., "Std__Path.cmo") *)
val cmo: t -> Path.t
(** Get the compiled object filename (e.g., "Std__Path.cmx") *)
val cmx: t -> Path.t
(** Get the native object filename (e.g., "Std__Path.o") *)
val o: t -> Path.t
(** Get the compiled typed tree filename (e.g., "Std__Path.cmt") *)
val cmt: t -> Path.t
(** Get the compiled interface typed tree filename (e.g., "Std__Path.cmti") *)
val cmti: t -> Path.t
(** Check if two modules are equal *)
val eq: t -> t -> bool
