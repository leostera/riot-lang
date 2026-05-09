open Std

module Executor = Executor
module Goal = Goal
module Goal_planner = Goal_planner
module Package_catalog = Package_catalog
module Package_work = Package_work
module Config = Build_config
module Build_result = Build_result
module Build_services = Build_services
module Action_executor = Action_executor
module Error = Error
module Event = Event
module Intent_planner = Intent_planner
module Module_plan = Module_plan
module Module_planning = Module_planning
module Package_finalizer = Package_finalizer
module Package_planning = Package_planning
module User_intent = User_intent
module Source_analyzer = Source_analyzer
module Toolchain_service = Toolchain_service
module Workspace_loader = Workspace_loader
module Work_graph = Work_graph
module Work_node = Work_node
module Work_registry = Work_registry

type t = Build_services.t

let create_executor: config:Config.t -> unit -> (t, Error.t) result = fun ~config () ->
  Ok (Build_services.create ~config ())

let execute: t -> User_intent.t -> (Build_result.t, Error.t) result = fun t intent ->
  let config = Build_services.config t in
  let seed = Work_node.user_intent ~id:(Work_node.Node_id.from_int 1) intent in
  let summary = Executor.run ~config ~seeds:[ seed ] ~execute:(Build_services.execute_node t) () in
  let packages = Build_services.package_results t in
  Ok Build_result.{ packages; summary }

let build = fun ~config intent ->
  let open Std.Result.Syntax in
  let* executor = create_executor ~config () in
  execute executor intent
