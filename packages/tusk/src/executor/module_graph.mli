(** Module Graph - Intra-package dependency graph for building a single package *)

open Std
open Core
open Model

(** Module graph type - opaque *)
type t

(** Error type *)
type error = string

(** Build a module graph for a package

    @param node The build node containing package info
    @param workspace The workspace context
    @return Result containing the module graph and compilation actions
*)
val build :
  node:Build_node.t ->
  workspace:Workspace.t ->
  (t * Actions.action list, error) result