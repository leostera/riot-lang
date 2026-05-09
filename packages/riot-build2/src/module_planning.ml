open Std
open Std.Result.Syntax

module ConcurrentHashMap = Collections.ConcurrentHashMap

type t = {
  workspace: Riot_model.Workspace.t;
  catalog: Package_catalog.t;
  store: Riot_store.Store.t;
  package_planning: Package_planning.t;
  source_analyzer: Source_analyzer.t;
  plans: (Goal.build_package, Module_plan.t) ConcurrentHashMap.t;
}

let create = fun ~workspace ~catalog ~store ~package_planning ~source_analyzer () ->
  {
    workspace;
    catalog;
    store;
    package_planning;
    source_analyzer;
    plans = ConcurrentHashMap.with_capacity ~size:128;
  }

let find = fun t build -> ConcurrentHashMap.get t.plans ~key:build

let path_error_message = fun __tmp1 ->
  match __tmp1 with
  | Path.InvalidUtf8 { path } -> "invalid utf8 path: " ^ path
  | Path.SystemInvalidUtf8 { syscall; path } -> syscall ^ " returned invalid utf8 path: " ^ path
  | Path.SystemError message -> message

let absolute_path = fun path ->
  if Path.is_absolute path then
    Ok path
  else
    Env.current_dir ()
    |> Result.map ~fn:(fun cwd -> Path.normalize Path.(cwd / path))
    |> Result.map_err
      ~fn:(fun error ->
        Error.ExecutorInvariantViolated {
          message = "failed to resolve current directory: " ^ path_error_message error;
        })

let source_groups = fun (package: Riot_model.Package.t) ->
  let source_dir = Path.v "src" in
  if List.is_empty package.sources.src then
    []
  else
    let root_mode =
      match package.library with
      | Some _ ->
          Riot_planner.Module_graph.Library_root {
            library_name = Riot_model.Package_name.to_string package.name;
          }
      | None -> Riot_planner.Module_graph.Loose_sources
    in
    [
      Riot_planner.Module_graph.{
        source_dir;
        allowed_source_files = package.sources.src;
        root_mode;
        namespace = Riot_model.Namespace.empty;
      };
    ]

let package_source_tasks = fun (t: t) ~(package:Riot_model.Package.t) ~toolchain ~build_ctx ->
  let config =
    Riot_planner.Module_graph.{
      root = package.path;
      source_groups = source_groups package;
      package;
      toolchain;
      workspace = t.workspace;
    }
  in
  let graph_builder = Riot_planner.Module_graph.create config in
  Riot_planner.Module_graph.source_tasks graph_builder

let realized_dependency_packages = fun t ~scope ~intent (package: Riot_model.Package.t) ->
  let rec loop acc = fun __tmp1 ->
    match __tmp1 with
    | [] -> Ok (List.reverse acc)
    | dependency :: rest ->
        if Riot_model.Package.is_builtin_dependency dependency then
          loop acc rest
        else
          (
            match Package_catalog.realize t.catalog ~intent dependency.name with
            | Ok dependency_package -> loop (dependency_package :: acc) rest
            | Error (Error.MissingPackage _) ->
                Error (Error.ExternalDependencyUnsupported {
                  package = package.name;
                  dependency = dependency.name;
                })
            | Error error -> Error error
          )
  in
  loop [] (Riot_model.Package.dependencies_for_scope scope package)

let plan = fun t registry (build: Goal.build_package) ->
  let* input = Package_planning.resolve t.package_planning build in
  let package = input.package in
  let tasks =
    package_source_tasks t ~package ~toolchain:input.toolchain ~build_ctx:input.build_ctx
  in
  let missing = Source_analyzer.missing t.source_analyzer ~package:package.name tasks in
  if not (List.is_empty missing) then
    Ok (
      Work_result.RequeueWithDependencies (
        List.map
          missing
          ~fn:(fun source ->
            ignore (Work_registry.intern_source_analysis registry source);
            Work_node.SourceAnalysisKey source.Source_analysis.key)
      )
    )
  else
    let* dependency_packages =
      realized_dependency_packages
        t
        ~scope:(Goal.dependency_scope build.scope)
        ~intent:Riot_model.Package.Runtime
        package
    in
    let planner_input =
      Riot_planner.Module_planner.{
        package;
        profile = input.profile;
        ctx = input.build_ctx;
        toolchain = input.toolchain;
        workspace = t.workspace;
        source_groups = source_groups package;
        depset = [];
        dependency_packages;
        store = t.store;
        on_source_analyzed = (fun _ -> ());
      }
    in
    match Riot_planner.Module_planner.plan_node
      ~analyze_sources:(Source_analyzer.analyze_from_cache t.source_analyzer package)
      planner_input with
    | Error error ->
        Error (Error.ModulePlanningFailed {
          package = package.name;
          reason = Riot_planner.Planning_error.to_string error;
        })
    | Ok planned ->
        let action_nodes = Riot_planner.Action_graph.nodes planned.action_graph in
        let sandbox_dir =
          Path.(Riot_model.Riot_dirs.sandbox_dir_in_workspace
            ~workspace:t.workspace
            ~profile:input.profile.name
            ~target:input.target
          / Path.v (Riot_model.Package_name.to_string package.name)
          / Path.v (Crypto.Digest.hex input.package_hash))
        in
        let* sandbox_dir = absolute_path sandbox_dir in
        let module_plan =
          Module_plan.{
            build;
            package;
            profile = input.profile;
            target = input.target;
            toolchain = input.toolchain;
            build_ctx = input.build_ctx;
            action_graph = planned.action_graph;
            action_nodes;
            sandbox_dir;
            package_hash = input.package_hash;
          }
        in
        ignore (ConcurrentHashMap.insert t.plans ~key:build ~value:module_plan);
        Ok (Work_result.Complete [])

let execute = fun t registry (build: Goal.build_package) ->
  match find t build with
  | Some _ -> Ok (Work_result.Complete [])
  | None ->
      if Package_planning.toolchain_ready t.package_planning build.target then
        plan t registry build
      else
        Ok (Work_result.RequeueWithDependencies [
          Work_node.ToolchainReadyKey { target = build.target };
        ])
