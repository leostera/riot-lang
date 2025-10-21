open Std
open Tusk_model

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

let plan_workspace ~workspace ~target =
  Workspace_planner.plan_workspace ~workspace
    ~target:
      (match target with
      | All -> Workspace_planner.All
      | Package name -> Workspace_planner.Package name)

let plan_package ~workspace ~toolchain ~package =
  let planning_root = Path.v "src" in
  let dependencies = [] in
  let plan_input =
    { Planner.package; toolchain; workspace; planning_root; dependencies }
  in
  Planner.plan_node plan_input

module Action = Action
module Action_node = Action_node
module Action_graph = Action_graph
module Module_node = Module_node
module Module_registry = Module_registry
module Module_scanner = Module_scanner
module Graph_builder = Graph_builder
module Alias_module = Alias_module
module Library_interface = Library_interface
module Library_definition = Library_definition
module Dependency = Dependency
module Planning_error = Planning_error
module Package_graph = Package_graph
module Workspace_planner = Workspace_planner
module Planner = Planner
