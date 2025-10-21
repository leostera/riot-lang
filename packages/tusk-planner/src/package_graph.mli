open Std
open Tusk_model

type t

exception Cycle_detected of string list

val create : Workspace.t -> t
(** Create a package dependency graph from a workspace. Each package becomes a
    node, edges represent dependencies. *)

val size : t -> int
(** Return the number of packages in the graph *)

val filter_for_package : t -> string -> t
(** Filter the graph to only include the specified package and its transitive
    dependencies. Returns an empty graph if package not found. *)

val topological_sort : t -> Package.t list
(** Return packages in topological order (dependencies before dependents).
    Raises Cycle_detected if there are circular dependencies. *)

val packages : t -> Package.t list
(** Return all packages in the graph in arbitrary order *)

val find_package : t -> string -> Package.t option
(** Find a package by name *)

val get_dependencies : t -> Package.t -> Package.t list
(** Get direct dependencies of a package *)
