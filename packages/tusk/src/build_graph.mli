(** Build graph - Dependency graph construction and analysis

    This module manages the build dependency graph, providing topological
    sorting, hash computation, and graph filtering capabilities. *)

type node = Build_node.t
(** Build node type from Build_node module *)

type t
(** Build graph containing nodes and their relationships *)

(** {1 Graph Construction} *)

val create : Workspace.t -> Toolchains.toolchain -> t
(** Create a build graph from a workspace. Constructs nodes for all packages and
    establishes dependency relationships. *)

(** {1 Graph Analysis} *)

val topological_sort : t -> node list
(** Perform topological sort using Kahn's algorithm. Returns nodes in build
    order (dependencies first). Raises [Failure] if circular dependencies are
    detected. *)

val filter_for_package : t -> string -> t
(** Filter the graph to include only the target package and its dependencies.
    Creates a new graph containing only the necessary nodes. *)

(** {1 Graph Visualization} *)

val print : t -> unit
(** Print the build graph to stdout. Shows build order and dependency
    relationships. *)
