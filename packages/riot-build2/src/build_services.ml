open Std
open Std.Result.Syntax

type t = {
  config: Build_config.t;
  catalog: Package_catalog.t;
  module_providers: Module_provider_registry.t;
  toolchains: Toolchain_service.t;
  package_planning: Package_planning.t;
  package_sandbox: Package_sandbox.t;
  source_analyzer: Source_analyzer.t;
  module_planning: Module_planning.t;
  action_executor: Action_executor.t;
  rule_service: Rule_service.t;
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
  let package_sandbox = Package_sandbox.create ~workspace ~store () in
  let source_analyzer = Source_analyzer.create ~store () in
  let module_planning =
    Module_planning.create
      ~workspace
      ~catalog
      ~store
      ~package_planning
      ~package_sandbox
      ~module_providers
      ~source_analyzer
      ()
  in
  let action_executor = Action_executor.create ~store ~toolchains () in
  let rule_service =
    Rule_service.create
      ~workspace
      ~catalog
      ~store
      ~package_planning
      ~package_sandbox
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
    package_sandbox;
    source_analyzer;
    module_planning;
    action_executor;
    rule_service;
  }

let begin_execution = fun t ->
  Package_catalog.begin_execution t.catalog;
  Package_sandbox.begin_execution t.package_sandbox;
  Module_planning.begin_execution t.module_planning;
  Rule_service.begin_execution t.rule_service

let config = fun t -> t.config

let catalog = fun t -> t.catalog

let action_results = fun t -> Action_executor.results t.action_executor

let module_plan = fun t build -> Module_planning.find t.module_planning build

let package_results = fun t -> Rule_service.package_results t.rule_service

let plan_dependencies = fun t registry node ->
  match Work_node.kind node with
  | Work_node.UserIntent intent ->
      Intent_planner.expand t.catalog intent
      |> Result.map ~fn:(fun goals ->
        List.map goals ~fn:(fun goal -> Work_request.existing (Work_node.GoalKey goal)))
  | Goal (BuildPackage build) ->
      Rule_service.plan_goal_dependencies t.rule_service build
  | Goal _
  | ToolchainReady _
  | SourceAnalysis _ -> Ok []
  | PackageArtifact build ->
      Rule_service.plan_package_artifact_dependencies t.rule_service build
  | ModuleDependencies build ->
      Rule_service.plan_module_dependencies t.rule_service build
  | OCamlArchive build ->
      Rule_service.plan_ocaml_archive t.rule_service build
  | OCamlInterface source
  | OCamlByteImplementation source
  | OCamlImplementation source ->
      Rule_service.plan_ocaml_source t.rule_service source
  | OCamlGenerated source ->
      Rule_service.plan_ocaml_generated t.rule_service source
  | CObject c_object ->
      Rule_service.plan_c_object t.rule_service c_object
  | PackageFinalize build ->
      Error (Error.ExecutorInvariantViolated {
        message = "legacy package finalize node was planned in rule-based build service for "
        ^ Riot_model.Package_name.to_string build.package;
      })
  | ModulePlan build -> Module_planning.plan_dependencies t.module_planning registry build
  | ActionPlan build -> Ok [ Work_request.existing (Work_node.ModulePlanKey build) ]
  | OCamlLibrary action
  | ActionExecution action ->
      if Action_executor.requires_toolchain action then
        Ok (Work_request.from_keys [ Work_node.ToolchainReadyKey { target = action.ref_.target } ])
      else
        Ok []
let execute_node = fun t registry node ->
  match Work_node.kind node with
  | Work_node.UserIntent _ ->
      Error (Error.ExecutorInvariantViolated {
        message = "virtual work node reached concrete execution";
      })
  | Goal (BuildPackage _build) -> Ok (Work_result.Complete [])
  | Goal goal -> Error (Error.UnsupportedGoal { goal })
  | ToolchainReady toolchain ->
      Toolchain_service.ensure t.toolchains toolchain
      |> Result.map ~fn:(fun () -> Work_result.Complete [])
  | SourceAnalysis source ->
      Source_analyzer.execute t.source_analyzer source
      |> Result.map ~fn:(fun () -> Work_result.Complete [])
  | PackageArtifact build -> Rule_service.execute_package_artifact t.rule_service registry build
  | ModuleDependencies build ->
      Rule_service.execute_module_dependencies t.rule_service registry build
  | OCamlArchive build -> Rule_service.execute_ocaml_archive t.rule_service registry build
  | OCamlInterface source -> Rule_service.execute_ocaml_interface t.rule_service registry source
  | OCamlByteImplementation source ->
      Rule_service.execute_ocaml_byte_implementation t.rule_service registry source
  | OCamlImplementation source ->
      Rule_service.execute_ocaml_implementation t.rule_service registry source
  | OCamlGenerated source ->
      Rule_service.execute_ocaml_generated t.rule_service registry source
  | CObject c_object -> Rule_service.execute_c_object t.rule_service registry c_object
  | PackageFinalize build ->
      Error (Error.ExecutorInvariantViolated {
        message = "legacy package finalize node reached rule-based build service for "
        ^ Riot_model.Package_name.to_string build.package;
      })
  | ModulePlan build -> Module_planning.execute t.module_planning registry build
  | ActionPlan build ->
      Error (Error.ExecutorInvariantViolated {
        message = "legacy action plan node reached rule-based build service for "
        ^ Riot_model.Package_name.to_string build.package;
      })
  | OCamlLibrary action
  | ActionExecution action -> Action_executor.execute t.action_executor action
