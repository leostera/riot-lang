open Std
open Std.Result.Syntax

type t = {
  config: Build_config.t;
  catalog: Package_catalog.t;
  toolchains: Toolchain_service.t;
  package_planning: Package_planning.t;
  source_analyzer: Source_analyzer.t;
  module_planning: Module_planning.t;
  action_executor: Action_executor.t;
  package_finalizer: Package_finalizer.t;
}

let create = fun ~config () ->
  let workspace = config.Build_config.workspace in
  let catalog = Package_catalog.create workspace in
  let store = Riot_store.Store.create ~workspace in
  let session_id = Riot_model.Session_id.make () in
  let toolchains = Toolchain_service.create ~root:workspace.root () in
  let package_planning =
    Package_planning.create
      ~workspace
      ~catalog
      ~store
      ~session_id
      ~parallelism:config.parallelism
      ~toolchains
      ()
  in
  let source_analyzer = Source_analyzer.create () in
  let module_planning =
    Module_planning.create ~workspace ~catalog ~store ~package_planning ~source_analyzer ()
  in
  let action_executor = Action_executor.create ~store ~toolchains () in
  let package_finalizer =
    Package_finalizer.create
      ~workspace
      ~catalog
      ~store
      ~package_planning
      ~module_planning
      ~action_executor
      ()
  in
  {
    config;
    catalog;
    toolchains;
    package_planning;
    source_analyzer;
    module_planning;
    action_executor;
    package_finalizer;
  }

let config = fun t -> t.config

let catalog = fun t -> t.catalog

let package_results = fun t -> Package_finalizer.results t.package_finalizer

let plan_dependencies = fun t registry node ->
  match Work_node.kind node with
  | Work_node.UserIntent intent ->
      Intent_planner.expand t.catalog intent
      |> Result.map ~fn:(fun goals -> List.map goals ~fn:(fun goal -> Work_node.GoalKey goal))
  | Goal (BuildPackage build) ->
      Package_finalizer.plan_dependencies t.package_finalizer registry build
  | Goal _
  | ToolchainReady _
  | SourceAnalysis _ -> Ok []
  | ModulePlan build -> Module_planning.plan_dependencies t.module_planning registry build
  | ActionExecution action ->
      Ok (List.map action.dependencies ~fn:(fun ref_ -> Work_node.ActionExecutionKey ref_))

let execute_node = fun t registry node ->
  match Work_node.kind node with
  | Work_node.UserIntent _ ->
      Error (Error.ExecutorInvariantViolated {
        message = "virtual work node reached concrete execution";
      })
  | Goal (BuildPackage build) -> Package_finalizer.execute t.package_finalizer registry build
  | Goal goal -> Error (Error.UnsupportedGoal { goal })
  | ToolchainReady toolchain ->
      Toolchain_service.ensure t.toolchains toolchain
      |> Result.map ~fn:(fun () -> Work_result.Complete [])
  | SourceAnalysis source ->
      Source_analyzer.execute t.source_analyzer source
      |> Result.map ~fn:(fun () -> Work_result.Complete [])
  | ModulePlan build -> Module_planning.execute t.module_planning registry build
  | ActionExecution action -> Action_executor.execute t.action_executor action
