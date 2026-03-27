open Std
open Tusk_model

type t
type build_status = Cached | Fresh
type build_scope = Build | Runtime | Dev
type package_scope = build_scope

type package_node =
  | Unplanned of { package : Package.t; scope : package_scope }
  | Planned of {
      package : Package.t;
      scope : package_scope;
      module_graph : Module_node.t Graph.SimpleGraph.t;
      action_graph : Action_graph.t;
      hash : Std.Crypto.hash;
    }
  | Built of {
      package : Package.t;
      scope : package_scope;
      module_graph : Module_node.t Graph.SimpleGraph.t;
      action_graph : Action_graph.t;
      hash : Std.Crypto.hash;
      artifact : Tusk_store.Artifact.t;
      status : build_status;
      depset : Dependency.t list;
    }
  | Failed of {
      package : Package.t;
      scope : package_scope;
      hash : Std.Crypto.hash;
      error : string;
    }
  | Skipped of { package : Package.t; scope : package_scope; reason : string }

exception Cycle_detected of string list

type missing_dependency = {
  package : string;
  dependency : string;
}

type create_error = 
  | MissingPackages of { missing : missing_dependency list }

val create : scope:build_scope -> Workspace.t -> (t, create_error) result
(** Create a package dependency graph from a workspace. Each package becomes a
    node, edges represent dependencies. All nodes start as Unplanned.
    
    Returns Error(MissingPackages) if any package depends on packages that are
    not in the workspace. *)

val get_package : package_node -> Package.t
(** Extract the Package.t from a package_node *)

val get_scope : package_node -> package_scope
(** Extract the scope for a package_node *)

val package_key : package_name:string -> package_scope -> Package.key
(** Stable string key for a scoped package node *)

val get_key : package_node -> Package.key
(** Stable string key for a package_node *)

val is_planned : package_node -> bool
(** Check if a package node has been planned *)

val get_hash : package_node -> Std.Crypto.hash option
(** Get the hash of a planned package node, or None if unplanned *)

val get_unplanned_dependencies : t -> Package.t -> Package.t list
(** Get all direct dependencies that have not been planned yet *)

val mark_planned :
  t ->
  Package.key ->
  module_graph:Module_node.t Graph.SimpleGraph.t ->
  action_graph:Action_graph.t ->
  hash:Std.Crypto.hash ->
  unit
(** Mark a package as planned with its module graph, action graph, and hash *)

val size : t -> int
(** Return the number of packages in the graph *)

val filter_for_package : t -> string -> t
(** Filter the graph to only include the specified package and its transitive
    dependencies. Returns an empty graph if package not found. *)

val filter_for_packages : t -> string list -> t
(** Filter the graph to include the specified packages and all of their
    transitive dependencies. Returns an empty graph if none of the packages
    are found. *)

val topological_sort : t -> package_node list
(** Return packages in topological order (dependencies before dependents).
    Raises Cycle_detected if there are circular dependencies. *)

val packages : t -> Package.t list
(** Return all packages in the graph in arbitrary order *)

val find_package : t -> string -> Package.t option
(** Find a package by name *)

val get_node : t -> Package.t -> package_node Graph.SimpleGraph.node option
(** Get the graph node for a package *)

val get_node_by_key :
  t -> Package.key -> package_node Graph.SimpleGraph.node option
(** Get the graph node for a scoped package key *)

val get_package_node : t -> Package.t -> package_node option
(** Get the package_node value for a package *)

val get_dependencies : t -> Package.t -> package_node list
(** Get direct dependency package_node values of a package *)

val get_dependencies_for_node :
  t -> package_node Graph.SimpleGraph.node -> package_node list
(** Get direct dependency package_node values of a specific scoped node *)

val get_graph_node :
  t -> Graph.SimpleGraph.Node_id.t -> package_node Graph.SimpleGraph.node option
(** Lookup a graph node by id *)

val iter_nodes : t -> fn:(package_node Graph.SimpleGraph.node -> unit) -> unit
(** Iterate over all nodes in the graph *)
