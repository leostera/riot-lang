open Std
open Riot_model

type t
type build_status =
  | Cached
  | Fresh
type build_scope =
  | Build
  | Runtime
  | Dev
type dev_artifacts = Riot_model.Package.dev_artifacts = {
  tests: bool;
  examples: bool;
  benches: bool;
}
type package_scope = build_scope
type package_node =
  | Unplanned of {
      package: Package.t;
      scope: package_scope;
    }
  | Planned of {
      package: Package.t;
      scope: package_scope;
      module_graph: Module_node.t Graph.SimpleGraph.t;
      action_graph: Action_graph.t;
      hash: Std.Crypto.hash;
    }
  | Cached of {
      package: Package.t;
      scope: package_scope;
      hash: Std.Crypto.hash;
      artifact: Riot_store.Artifact.t;
      depset: Dependency.t list;
      exports: Riot_store.Store.export_entry list;
    }
  | Built of {
      package: Package.t;
      scope: package_scope;
      module_graph: Module_node.t Graph.SimpleGraph.t;
      action_graph: Action_graph.t;
      hash: Std.Crypto.hash;
      artifact: Riot_store.Artifact.t;
      status: build_status;
      depset: Dependency.t list;
    }
  | Failed of {
      package: Package.t;
      scope: package_scope;
      hash: Std.Crypto.hash;
      error: string;
    }
  | Skipped of {
      package: Package.t;
      scope: package_scope;
      reason: string;
    }

exception Cycle_detected of string list

type missing_dependency = { package: string; dependency: string }
(**
   Create a package dependency graph from a workspace. Each package becomes a
   node, edges represent dependencies. All nodes start as Unplanned.

   Returns Error(MissingPackages) if any package depends on packages that are
   not in the workspace.
*)
type create_error =
  | MissingPackages of {
      missing: missing_dependency list;
    }
type create_breakdown = {
  build_node_realization_count: int;
  build_node_realization_duration: Time.Duration.t;
  runtime_node_realization_count: int;
  runtime_node_realization_duration: Time.Duration.t;
  dev_node_realization_count: int;
  dev_node_realization_duration: Time.Duration.t;
  edge_wiring_duration: Time.Duration.t;
}

val create:
  scope:build_scope ->
  ?dev_artifacts:dev_artifacts ->
  ?dev_roots:Package_name.t list ->
  Workspace.t ->
  (t, create_error) result

val create_with_breakdown:
  scope:build_scope ->
  ?dev_artifacts:dev_artifacts ->
  ?dev_roots:Package_name.t list ->
  Workspace.t ->
  (t * create_breakdown, create_error) result

(** Clone a package graph so callers can mutate package status independently. *)
val clone: t -> t

(** Extract the Package.t from a package_node *)
val get_package: package_node -> Package.t

(** Extract the scope for a package_node *)
val get_scope: package_node -> package_scope

(** Stable string key for a scoped package node *)
val package_key: package_name:string -> package_scope -> Package.key

(** Stable string key for a package_node *)
val get_key: package_node -> Package.key

(** Check if a package node has been planned *)
val is_planned: package_node -> bool

(** Get the hash of a planned package node, or None if unplanned *)
val get_hash: package_node -> Std.Crypto.hash option

(** Get all direct dependencies that have not been planned yet *)
val get_unplanned_dependencies: t -> Package.t -> Package.t list

(** Mark a package as planned with its module graph, action graph, and hash *)
val mark_planned:
  t ->
  Package.key ->
  module_graph:Module_node.t Graph.SimpleGraph.t ->
  action_graph:Action_graph.t ->
  hash:Std.Crypto.hash ->
  unit

(** Return the number of packages in the graph *)
val size: t -> int

(**
   Filter the graph to only include the specified package and its transitive
   dependencies. Returns an empty graph if package not found.
*)
val filter_for_package: t -> Riot_model.Package_name.t -> t

(**
   Filter the graph to include the specified packages and all of their
   transitive dependencies. Returns an empty graph if none of the packages
   are found.
*)
val filter_for_packages: t -> Riot_model.Package_name.t list -> t

(**
   Return packages in topological order (dependencies before dependents).
   Raises Cycle_detected if there are circular dependencies.
*)
val topological_sort: t -> package_node list

(** Return all packages in the graph in arbitrary order *)
val packages: t -> Package.t list

(** Find a package by name *)
val find_package: t -> Riot_model.Package_name.t -> Package.t option

(** Get the graph node for a package *)
val get_node: t -> Package.t -> package_node Graph.SimpleGraph.node option

(** Get the graph node for a scoped package key *)
val get_node_by_key: t -> Package.key -> package_node Graph.SimpleGraph.node option

(** Get the package_node value for a package *)
val get_package_node: t -> Package.t -> package_node option

(** Get direct dependency package_node values of a package *)
val get_dependencies: t -> Package.t -> package_node list

(** Get direct dependency package_node values of a specific scoped node *)
val get_dependencies_for_node: t -> package_node Graph.SimpleGraph.node -> package_node list

(** Return direct runtime dependency packages for a package name. *)
val direct_runtime_dependencies: t -> Riot_model.Package_name.t -> Package.t list

(** Lookup a graph node by id *)
val get_graph_node: t -> Graph.SimpleGraph.Node_id.t -> package_node Graph.SimpleGraph.node option

(** Iterate over all nodes in the graph *)
val iter_nodes: t -> fn:(package_node Graph.SimpleGraph.node -> unit) -> unit
