(** A module name with namespace support *)
open Std

(** Abstract type representing a module name *)

(** Create a module name with explicit components *)
type t

val make: filename:Path.t -> namespace:Namespace.t -> name:string -> t

(** Create from a filename, optionally with namespace *)
val from_filename: ?namespace:Namespace.t -> Path.t -> t

(** Create from a string name, optionally with namespace *)
val from_string: ?namespace:Namespace.t -> string -> t

(** Create from a file path, extracting the module name *)
val from_path: Path.t -> t

(** Get the original filename *)
val filename: t -> Path.t

(** Get the simple module name (without namespace) *)
val to_string: t -> string

(** Get the namespace *)
val namespace: t -> Namespace.t

val simple_name: t -> string

(** Get the fully qualified name (namespace__name) *)
val qualified_name: t -> string

(** Get the .cma filename based on qualified name *)
val cma: t -> Path.t

(** Get the .cmxa filename based on qualified name *)
val cmxa: t -> Path.t

(** Get the .cmxs filename based on qualified name *)
val cmxs: t -> Path.t

(** Get the .cmo filename based on qualified name *)
val cmo: t -> Path.t

(** Get the .cmi filename based on qualified name *)
val cmi: t -> Path.t

(** Get the .cmx filename based on qualified name *)
val cmx: t -> Path.t

(** Get the .cmt filename based on qualified name *)
val cmt: t -> Path.t

(** Get the .cmti filename based on qualified name *)
val cmti: t -> Path.t

(** Get the .o filename based on qualified name *)
val o: t -> Path.t

(** Get the .a filename based on qualified name *)
val a: t -> Path.t

(** Get the canonical .mli filename *)
val canonical_mli: t -> Path.t

(** Get the canonical .ml filename *)
val canonical_ml: t -> Path.t

(** Get the binary name (qualified name without extension, e.g., "demo_cmd" or "namespace__demo_cmd") *)
val binary: t -> string
