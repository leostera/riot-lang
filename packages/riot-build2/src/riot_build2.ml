open Std

module Executor = Executor
module Goal = Goal
module Goal_planner = Goal_planner
module Package_catalog = Package_catalog
module Package_work = Package_work
module Build_request = Build_request
module Build_result = Build_result
module Build_services = Build_services
module Error = Error
module Event = Event
module Intent_planner = Intent_planner
module User_intent = User_intent
module Workspace_loader = Workspace_loader
module Work_graph = Work_graph
module Work_node = Work_node
module Work_registry = Work_registry

let build = fun (request: Build_request.t) ->
  let parallelism =
    match request.parallelism with
    | Some parallelism -> parallelism
    | None -> Thread.available_parallelism
  in
  let services = Build_services.create ~workspace:request.workspace ~parallelism () in
  let intent =
    User_intent.build
      ~packages:request.packages
      ~targets:request.targets
      ~profile:request.profile
      ()
  in
  let seed = Work_node.user_intent ~id:(Work_node.Node_id.of_int 1) intent in
  let summary =
    Executor.run ~parallelism ~seeds:[ seed ] ~execute:(Build_services.execute_node services) ()
  in
  { Build_result.packages = Build_services.package_results services; summary }
