open Std
open Tusk_model

type build_target = Workspace_planner.target
type plan_error = Workspace_planner.plan_error
type workspace_plan_result = Workspace_planner.package_plan
type module_plan_result = Module_planner.plan_result
type package_plan_result = Package_planner.plan_result

let plan_workspace ~workspace ~target =
  Workspace_planner.plan_workspace ~workspace ~target

let plan_package_with_graph ~workspace ~toolchain ~package_graph ~package =
  Package_planner.plan_package ~workspace ~toolchain ~package_graph ~package

let plan_package ~workspace ~toolchain ~package =
  let planning_root = Path.v "src" in
  let dependencies = [] in
  let plan_input =
    {
      Module_planner.package;
      toolchain;
      workspace;
      planning_root;
      dependencies;
    }
  in
  Module_planner.plan_node plan_input

module Action = Action
module Action_node = Action_node
module Action_graph = Action_graph
module Module_node = Module_node
module Module_registry = Module_registry
module Module_scanner = Module_scanner
module Module_graph = Module_graph
module Alias_module = Alias_module
module Library_interface = Library_interface
module Library_definition = Library_definition
module Dependency = Dependency
module Planning_error = Planning_error
module Package_graph = Package_graph
module Workspace_planner = Workspace_planner
module Module_planner = Module_planner
module Package_planner = Package_planner
