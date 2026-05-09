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

let manifest_dependencies_for_scope = fun scope (package: Riot_model.Package_manifest.t) ->
  match scope with
  | Riot_model.Package.Normal -> package.dependencies
  | Riot_model.Package.Dev -> package.dependencies @ package.dev_dependencies
  | Riot_model.Package.Build -> package.build_dependencies

let package_dependency_names = fun t ~scope (package: Riot_model.Package_manifest.t) ->
  let rec loop acc = fun __tmp1 ->
    match __tmp1 with
    | [] -> Ok (List.reverse acc)
    | dependency :: rest ->
        if Riot_model.Package.is_builtin_dependency dependency then
          loop acc rest
        else
          (
            match Package_catalog.find_manifest t.catalog dependency.name with
            | Some _ -> loop (dependency.name :: acc) rest
            | None ->
                Error (Error.ExternalDependencyUnsupported {
                  package = package.name;
                  dependency = dependency.name;
                })
          )
  in
  loop [] (manifest_dependencies_for_scope scope package)

let package_dependency_work = fun t (build: Package_work.build_library) ->
  let* package = Package_catalog.require_manifest t.catalog build.Package_work.package in
  let* dependencies =
    package_dependency_names t ~scope:(Package_work.dependency_scope build.scope) package
  in
  Ok (List.map
    dependencies
    ~fn:(fun package ->
      Work_node.PackageWorkKey (Package_work.BuildLibrary {
        package;
        scope = build.scope;
        profile = build.profile;
        target = build.target;
      })))

let execute_build_library_work = fun t build ->
  match Package_finalizer.find t.package_finalizer build with
  | Some _ -> Ok []
  | None ->
      let* dependency_work = package_dependency_work t build in
      Ok (dependency_work
      @ [
        Work_node.ToolchainReadyKey { target = build.target };
        Work_node.PackageFinalizeKey build;
      ])

let package_work_dependencies = fun t work ->
  match work with
  | Package_work.BuildLibrary build -> execute_build_library_work t build
  | TestPackage _
  | RunBinary _ -> Error (Error.UnsupportedPackageWork { work })

let goal_dependencies = fun t goal ->
  Goal_planner.expand t.catalog goal
  |> Result.map
    ~fn:(fun package_work -> List.map package_work ~fn:(fun work -> Work_node.PackageWorkKey work))

let compute_dependencies = fun t node ->
  match Work_node.kind node with
  | Work_node.UserIntent intent ->
      Intent_planner.expand intent
      |> List.map ~fn:(fun goal -> Work_node.GoalKey goal)
      |> Result.ok
  | Goal goal -> goal_dependencies t goal
  | PackageWork work -> package_work_dependencies t work
  | ToolchainReady _
  | SourceAnalysis _
  | ModulePlan _
  | PackageFinalize _
  | ActionExecution _ -> Ok []

let execute_node = fun t registry node ->
  match Work_node.kind node with
  | Work_node.UserIntent _
  | Goal _
  | PackageWork _ ->
      Error (Error.ExecutorInvariantViolated {
        message = "virtual work node reached concrete execution";
      })
  | ToolchainReady toolchain ->
      Toolchain_service.ensure t.toolchains toolchain
      |> Result.map ~fn:(fun () -> Work_result.Complete [])
  | SourceAnalysis source ->
      Source_analyzer.execute t.source_analyzer source
      |> Result.map ~fn:(fun () -> Work_result.Complete [])
  | ModulePlan build -> Module_planning.execute t.module_planning registry build
  | PackageFinalize build -> Package_finalizer.execute t.package_finalizer registry build
  | ActionExecution action -> Action_executor.execute t.action_executor action
