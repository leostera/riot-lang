open Std

module Executor = Executor
module Goal = Goal
module Action = Action
module Package_catalog = Package_catalog
module Config = Build_config
module Build_result = Build_result
module Dep_analysis = Dep_analysis
module Action_execution = Action_execution
module Action_timing_summary = Action_timing_summary
module Build_services = Build_services
module Action_executor = Action_executor
module Error = Error
module Event = Event
module Intent_planner = Intent_planner
module Module_plan = Module_plan
module Module_planning = Module_planning
module Module_provider_registry = Module_provider_registry
module Package_finalizer = Package_finalizer
module Package_planning = Package_planning
module Rule = Rule
module Rule_service = Rule_service
module User_intent = User_intent
module Source_analysis = Source_analysis
module Source_analyzer = Source_analyzer
module ExecutionSummary = ExecutionSummary
module Graph_cache = Graph_cache
module Toolchain_service = Toolchain_service
module Toolchain_ready = Toolchain_ready
module Source_analysis_cache = Source_analysis_cache
module Workspace_loader = Workspace_loader
module Work_graph = Work_graph
module Work_node = Work_node
module Work_request = Work_request
module Work_result = Work_result
module Work_registry = Work_registry

type t = Build_services.t

let create_executor: config:Config.t -> unit -> (t, Error.t) result = fun ~config () ->
  Ok (Build_services.create ~config ())

let execute: t -> User_intent.t -> (Build_result.t, Error.t) result = fun t intent ->
  Build_services.begin_execution t;
  let seed = Work_node.user_intent ~id:(Work_node.Node_id.from_int 1) intent in
  let summary = Executor.run ~services:t ~seeds:[ seed ] () in
  let packages = Build_services.package_results t in
  Ok Build_result.{ packages; summary }

let build = fun ~config intent ->
  let open Std.Result.Syntax in
  let* executor = create_executor ~config () in
  execute executor intent
