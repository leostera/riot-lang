(** Module Graph - Intra-package dependency graph for building a single package
*)

open Std
open Model

type t
(** Module graph type - opaque *)

type error = string
(** Error type *)

val build :
  node:Build_node.t ->
  workspace:Workspace.t ->
  build_graph:Build_graph.t ->
  (t * Actions.action list * Path.t list, error) result
(** Build a module graph for a package

    @param node The build node containing package info
    @param workspace The workspace context
    @param build_graph The build graph for dependency ordering
    @return Result containing the module graph and compilation actions *)
