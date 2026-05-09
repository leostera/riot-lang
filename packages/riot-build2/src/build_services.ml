open Std
open Std.Result.Syntax

type t = {
  catalog: Package_catalog.t;
  toolchains: Toolchain_service.t;
  source_analyzer: Source_analyzer.t;
  module_planning: Module_planning.t;
  action_executor: Action_executor.t;
  package_finalizer: Package_finalizer.t;
}

let create = fun ~workspace ?(parallelism = Thread.available_parallelism) () ->
  let parallelism = Int.max 1 parallelism in
  let catalog = Package_catalog.create workspace in
  let store = Riot_store.Store.create ~workspace in
  let session_id = Riot_model.Session_id.make () in
  let toolchains = Toolchain_service.create ~root:workspace.root () in
  let source_analyzer = Source_analyzer.create () in
  let module_planning =
    Module_planning.create
      ~workspace
      ~catalog
      ~store
      ~session_id
      ~parallelism
      ~toolchains
      ~source_analyzer
      ()
  in
  let action_executor = Action_executor.create ~store ~toolchains () in
  let package_finalizer =
    Package_finalizer.create ~workspace ~store ~module_planning ~action_executor ()
  in
  {
    catalog;
    toolchains;
    source_analyzer;
    module_planning;
    action_executor;
    package_finalizer;
  }

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
  let* dependencies = package_dependency_names t ~scope:Riot_model.Package.Normal package in
  Ok (List.map
    dependencies
    ~fn:(fun package ->
      Work_node.PackageWorkKey (Package_work.BuildLibrary {
        package;
        profile = build.profile;
        target = build.target;
      })))

let execute_build_library_work = fun t build ->
  match Package_finalizer.find t.package_finalizer build with
  | Some _ -> Ok (Executor.Complete [])
  | None ->
      let* dependency_work = package_dependency_work t build in
      Ok (Executor.RequeueWithDependencies (dependency_work
      @ [
        Work_node.ToolchainReadyKey { target = build.target };
        Work_node.PackageFinalizeKey build;
      ]))

let execute_package_work = fun t work ->
  match work with
  | Package_work.BuildLibrary build -> execute_build_library_work t build
  | TestPackage _
  | RunBinary _ -> Error (Error.UnsupportedPackageWork { work })

let execute_goal = fun t goal ->
  Goal_planner.expand t.catalog goal
  |> Result.map
    ~fn:(fun package_work ->
      Executor.Complete (List.map package_work ~fn:(fun work -> Work_node.PackageWorkKey work)))

let execute_node = fun t context node ->
  match Work_node.kind node with
  | Work_node.UserIntent intent ->
      let goals =
        Intent_planner.expand intent
        |> List.map ~fn:(fun goal -> Work_node.GoalKey goal)
      in
      Ok (Executor.Complete goals)
  | Goal goal -> execute_goal t goal
  | PackageWork work -> execute_package_work t work
  | ToolchainReady toolchain ->
      Toolchain_service.ensure t.toolchains toolchain
      |> Result.map ~fn:(fun () -> Executor.Complete [])
  | SourceAnalysis source ->
      Source_analyzer.execute t.source_analyzer source
      |> Result.map ~fn:(fun () -> Executor.Complete [])
  | ModulePlan build -> Module_planning.execute t.module_planning context.Executor.registry build
  | PackageFinalize build ->
      Package_finalizer.execute t.package_finalizer context.Executor.registry build
  | ActionExecution action -> Action_executor.execute t.action_executor action
