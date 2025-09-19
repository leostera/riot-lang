(** Dependency graph builder *)

type node = {
  id : Node_id.t;
  file : string;
  module_name : string;
  namespaced : string;
  mutable deps : Node_id.t list; (* Node IDs this depends on *)
}

type t = {
  nodes : (int, node) Hashtbl.t;
  registry : Module_registry.t;
  package_name : string;
}

val create : package_name:string -> t
(** Create a dependency graph for a package *)

val build : t -> string -> unit
(** Build dependency graph for files in directory *)

val to_dot : t -> Std.Graph.Dot.t
(** Convert to DOT format for visualization *)

val to_mermaid : t -> Std.Graph.Mermaid.t
(** Convert to Mermaid diagram format *)

val topological_sort : t -> node list
(** Return nodes in dependency order *)
