(** Build graph - Dependency graph construction and analysis
    
    This module manages the build dependency graph, providing topological
    sorting, hash computation, and graph filtering capabilities. *)

(** Build node type from Build_node module *)
type node = Build_node.t

(** Build graph containing nodes and their relationships *)
type t 

(** {1 Graph Construction} *)

(** Create a build graph from a workspace.
    Constructs nodes for all packages and establishes dependency relationships. *)
val create : Workspace.workspace -> Toolchains.toolchain -> t

(** {1 Graph Analysis} *)

(** Perform topological sort using Kahn's algorithm.
    Returns nodes in build order (dependencies first).
    Raises [Failure] if circular dependencies are detected. *)
val topological_sort : t -> node list

(** Filter the graph to include only the target package and its dependencies.
    Creates a new graph containing only the necessary nodes. *)
val filter_for_package : t -> string -> t

(** {1 Hash Computation} *)

(** Result type for hash computation *)
type hash_result = 
  | Ok of Hasher.hash
  | MissingDependencies of Build_node.t list
  | Error of string

(** Force recomputation of hash for a node, ignoring any cached value.
    Returns the newly computed hash. *)
val recompute_node_hash : Toolchains.toolchain -> node -> hash_result

(** Get hash for a node, computing it if necessary.
    Uses bottom-up traversal to ensure dependency hashes are available.
    Checks if dependency artifacts exist in the store. *)
val get_node_hash : Toolchains.toolchain -> node -> Store.t -> hash_result

(** {1 Graph Visualization} *)

(** Print the build graph to stdout.
    Shows build order and dependency relationships. *)
val print : t -> unit
