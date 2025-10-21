open Std
open Tusk_model

(** Top-level planner interface *)

type build_target = All | Package of string

type plan_error = Workspace_planner.plan_error =
  | PackageNotFound of { name : string; available : string list }
  | CycleDetected of { cycle : string list }

type workspace_plan_result = Workspace_planner.package_plan
(** Re-export types from Planner *)

type package_plan_result = Planner.plan_result = {
  module_graph : Module_node.t Graph.SimpleGraph.t;
  action_graph : Action_graph.t;
}

val plan_workspace :
  workspace:Workspace.t ->
  target:build_target ->
  (workspace_plan_result, plan_error) result
(** Plan the workspace - returns ordered packages to build. Does NOT plan
    module/action graphs - that's done on-demand per package. *)

val plan_package :
  workspace:Workspace.t ->
  toolchain:Tusk_toolchain.t ->
  package:Package.t ->
  (package_plan_result, Planning_error.t) result
(** Plan a single package on-demand - builds module graph and action graph.
    Called by executor when actually building a package (after cache miss). *)

module Action = Action
(** Sub-modules *)

module Action_graph = Action_graph
module Action_node = Action_node
module Alias_module = Alias_module
module Dependency = Dependency
module Graph_builder = Graph_builder
module Library_definition = Library_definition
module Library_interface = Library_interface
module Module_node = Module_node
module Module_registry = Module_registry
module Module_scanner = Module_scanner
module Planning_error = Planning_error
module Package_graph = Package_graph
module Workspace_planner = Workspace_planner
module Planner = Planner
