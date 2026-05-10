open Std
open Std.Result.Syntax

type t = {
  config: Build_config.t;
  catalog: Package_catalog.t;
  module_providers: Module_provider_registry.t;
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
  let module_providers = Module_provider_registry.create ~catalog () in
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
  let source_analyzer = Source_analyzer.create ~store () in
  let module_planning =
    Module_planning.create
      ~workspace
      ~catalog
      ~store
      ~package_planning
      ~module_providers
      ~source_analyzer
      ()
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
    module_providers;
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

let action_dependency_key = fun registry ref_ ->
  let library_key = Work_node.OCamlLibraryKey ref_ in
  if Option.is_some (Work_registry.find registry library_key) then
    library_key
  else
    let archive_key = Work_node.OCamlArchiveKey ref_ in
    if Option.is_some (Work_registry.find registry archive_key) then
      archive_key
    else
      Work_node.ActionExecutionKey ref_

let action_dependencies = fun registry action ->
  let action_dependencies =
    List.map action.Action_execution.dependencies ~fn:(action_dependency_key registry)
  in
  if Action_executor.requires_toolchain action then
    Work_node.ToolchainReadyKey { target = action.ref_.target } :: action_dependencies
  else
    action_dependencies

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
  | PackageArtifact build ->
      Package_finalizer.plan_artifact_dependencies t.package_finalizer registry build
  | PackageFinalize build ->
      Package_finalizer.plan_finalize_dependencies t.package_finalizer registry build
  | ModulePlan build -> Module_planning.plan_dependencies t.module_planning registry build
  | ActionPlan build ->
      Package_finalizer.plan_action_dependencies t.package_finalizer registry build
  | OCamlLibrary action
  | OCamlArchive action ->
      Ok (action_dependencies registry action)
  | ActionExecution action ->
      Ok (action_dependencies registry action)

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
  | PackageArtifact build -> Package_finalizer.execute_artifact t.package_finalizer registry build
  | PackageFinalize build -> Package_finalizer.execute_finalize t.package_finalizer registry build
  | ModulePlan build -> Module_planning.execute t.module_planning registry build
  | ActionPlan build -> Package_finalizer.execute_action_plan t.package_finalizer registry build
  | OCamlLibrary action
  | OCamlArchive action ->
      Action_executor.execute t.action_executor action
  | ActionExecution action -> Action_executor.execute t.action_executor action
