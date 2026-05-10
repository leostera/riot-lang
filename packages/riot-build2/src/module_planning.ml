open Std
open Std.Result.Syntax

module ConcurrentHashMap = Collections.ConcurrentHashMap

type t = {
  workspace: Riot_model.Workspace.t;
  catalog: Package_catalog.t;
  store: Riot_store.Store.t;
  package_planning: Package_planning.t;
  module_providers: Module_provider_registry.t;
  source_analyzer: Source_analyzer.t;
  module_plan_cache: Module_plan_cache.payload Graph_cache.t;
  plans: (Goal.build_package, Module_plan.t) ConcurrentHashMap.t;
}

let create = fun
  ~workspace ~catalog ~store ~package_planning ~module_providers ~source_analyzer () ->
  {
    workspace;
    catalog;
    store;
    package_planning;
    module_providers;
    source_analyzer;
    module_plan_cache = Module_plan_cache.create_cache ~store;
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

let package_dependency_keys = fun t build ->
  Module_provider_registry.dependency_keys_for_build
    t.module_providers
    build

let source_dependency_requests = fun t (build: Goal.build_package) ->
  let* input = Package_planning.resolve t.package_planning build in
  let package = input.package in
  let tasks =
    package_source_tasks t ~package ~toolchain:input.toolchain ~build_ctx:input.build_ctx
  in
  Ok (
    List.map
      tasks
      ~fn:(fun source ->
        let source = Source_analysis.make ~package:package.name ~task:source in
        Work_request.materialize (Work_node.SourceAnalysis source))
  )

let plan_dependencies = fun t _registry build ->
  let* package_dependencies = package_dependency_keys t build in
  let* source_dependencies = source_dependency_requests t build in
  Ok (Work_request.from_keys package_dependencies @ source_dependencies)

let sandbox_dir = fun t (input: Package_planning.input) ->
  Path.(Riot_model.Riot_dirs.sandbox_dir_in_workspace
    ~workspace:t.workspace
    ~profile:input.Package_planning.profile.name
    ~target:input.target
  / Path.v (Riot_model.Package_name.to_string input.package.name)
  / Path.v (Crypto.Digest.hex input.package_hash))
  |> absolute_path

let is_final_archive_action = fun __tmp1 ->
  match __tmp1 with
  | Action.CompileLibrary { sources = []; outputs; _ } ->
      List.any outputs ~fn:(fun output -> Path.extension output = Some ".cmxa")
  | _ -> false

let classify_action_executions = fun action_executions ->
  let ocaml_libraries =
    action_executions
    |> List.filter
      ~fn:(fun action ->
        match action.Action_execution.action with
        | Action.CompileSource _
        | Action.CompileSources _ -> true
        | _ -> false)
  in
  let ocaml_archive =
    action_executions
    |> List.find ~fn:(fun action -> is_final_archive_action action.Action_execution.action)
  in
  (ocaml_libraries, ocaml_archive)

let module_plan_from_actions = fun t (input: Package_planning.input) action_executions ->
  let* sandbox_dir = sandbox_dir t input in
  let (ocaml_libraries, ocaml_archive) = classify_action_executions action_executions in
  Ok Module_plan.{
    build = input.build;
    package = input.package;
    profile = input.profile;
    target = input.target;
    toolchain = input.toolchain;
    build_ctx = input.build_ctx;
    action_executions;
    ocaml_libraries;
    ocaml_archive;
    sandbox_dir;
    package_hash = input.package_hash;
  }

let load_cached_plan = fun t (input: Package_planning.input) ->
  match Graph_cache.get t.module_plan_cache input.Package_planning.package_hash with
  | None -> Ok None
  | Some (Error error) -> Error error
  | Some (Ok payload) ->
      let* sandbox_dir = sandbox_dir t input in
      let* action_executions =
        Module_plan_cache.action_executions
          ~package:input.package
          ~profile:input.profile
          ~target:input.target
          ~toolchain:input.toolchain
          ~sandbox_dir
          payload
      in
      let (ocaml_libraries, ocaml_archive) = classify_action_executions action_executions in
      let module_plan =
        Module_plan.{
          build = input.build;
          package = input.package;
          profile = input.profile;
          target = input.target;
          toolchain = input.toolchain;
          build_ctx = input.build_ctx;
          action_executions;
          ocaml_libraries;
          ocaml_archive;
          sandbox_dir;
          package_hash = input.package_hash;
        }
      in
      ignore (ConcurrentHashMap.insert t.plans ~key:input.build ~value:module_plan);
      Ok (Some module_plan)

let root_module_name_of_package_name = fun package_name ->
  Riot_model.Module_name.(from_string (Riot_model.Package_name.to_string package_name)
  |> to_string)

let direct_dependency_package_by_name = fun depset package_name ->
  depset
  |> List.find
    ~fn:(fun dep ->
      Riot_model.Package_name.equal
        dep.Riot_planner.Dependency.package.name
        package_name)
  |> Option.map ~fn:(fun dep -> dep.Riot_planner.Dependency.package)

let transitive_dependency_package_by_name = fun depset package_name ->
  Riot_planner.Dependency.transitive_closure depset
  |> List.find
    ~fn:(fun dep ->
      Riot_model.Package_name.equal
        dep.Riot_planner.Dependency.package.name
        package_name)
  |> Option.map ~fn:(fun dep -> dep.Riot_planner.Dependency.package)

let dependency_package_by_name = fun t dependency_packages depset package_name ->
  match direct_dependency_package_by_name depset package_name with
  | Some package -> Some package
  | None ->
      match transitive_dependency_package_by_name depset package_name with
      | Some package -> Some package
      | None ->
          List.find
            dependency_packages
            ~fn:(fun package ->
              Riot_model.Package_name.equal
                package.Riot_model.Package.name
                package_name)
          |> Option.or_else
            ~fn:(fun () ->
              match Package_catalog.realize
                t.catalog
                ~intent:Riot_model.Package.Runtime
                package_name with
              | Ok package -> Some package
              | Error _ -> None)

let build_module_graph = fun (t: t) (input: Package_planning.input) ~depset ~dependency_packages ->
  let config =
    Riot_planner.Module_graph.{
      root = input.package.path;
      source_groups = source_groups input.package;
      package = input.package;
      toolchain = input.toolchain;
      workspace = t.workspace;
    }
  in
  let graph_builder = Riot_planner.Module_graph.create config in
  List.for_each
    (Riot_model.Package.build_graph_dependencies input.package)
    ~fn:(fun dependency ->
      if Riot_model.Package.is_builtin_dependency dependency then
        Riot_planner.Module_graph.add_direct_dependency_root
          graph_builder
          ~package_name:dependency.name
          ~root_module:(root_module_name_of_package_name dependency.name)
      else
        match dependency_package_by_name t dependency_packages depset dependency.name with
        | Some package ->
            Riot_planner.Module_graph.add_direct_dependency_package graph_builder package
        | None ->
            Riot_planner.Module_graph.add_direct_dependency_root
              graph_builder
              ~package_name:dependency.name
              ~root_module:(root_module_name_of_package_name dependency.name));
  (
    match input.package.sources.native with
    | [] -> ()
    | files ->
        let native_node = Riot_planner.Module_node.make_native ~files in
        let _ =
          Std.Graph.SimpleGraph.add_node (Riot_planner.Module_graph.graph graph_builder) native_node
        in
        ()
  );
  match Riot_planner.Module_graph.wire_dependencies
    ~analyze_sources:(Source_analyzer.analyze_from_cache t.source_analyzer input.package)
    ~on_source_analyzed:(fun _ -> ())
    graph_builder with
  | Error error ->
      Error (Error.ModulePlanningFailed {
        package = input.package.name;
        reason = Riot_planner.Planning_error.to_string error;
      })
  | Ok () ->
      (
        match input.package.library with
        | Some _ ->
            Riot_planner.Module_graph.add_library_node
              graph_builder
              ~name:(Riot_model.Package_name.to_string input.package.name)
              ~includes:[]
        | None -> ()
      );
      Ok (Riot_planner.Module_graph.graph graph_builder)

let plan = fun t _registry (build: Goal.build_package) ->
  let* depset = Package_planning.depset t.package_planning build in
  let* input = Package_planning.resolve ~depset t.package_planning build in
  let package = input.package in
  let tasks =
    package_source_tasks t ~package ~toolchain:input.toolchain ~build_ctx:input.build_ctx
  in
  let missing = Source_analyzer.missing t.source_analyzer ~package:package.name tasks in
  if not (List.is_empty missing) then
    Error (Error.ExecutorInvariantViolated {
      message = "module planning for "
      ^ Riot_model.Package_name.to_string package.name
      ^ " started before source analysis dependencies completed";
    })
  else
    let* dependency_packages =
      realized_dependency_packages
        t
        ~scope:(Goal.dependency_scope build.scope)
        ~intent:Riot_model.Package.Runtime
        package
    in
    let* module_graph = build_module_graph t input ~depset ~dependency_packages in
    let* sandbox_dir = sandbox_dir t input in
    let* action_executions =
      Action_planner.plan
        {
          Action_planner.package = input.package;
          profile = input.profile;
          target = input.target;
          build_ctx = input.build_ctx;
          toolchain = input.toolchain;
          depset;
          sandbox_dir;
          module_graph;
        }
    in
    let* module_plan = module_plan_from_actions t input action_executions in
    let* () =
      Graph_cache.put
        t.module_plan_cache
        input.package_hash
        (Module_plan_cache.payload_of_plan module_plan)
    in
    let _ = ConcurrentHashMap.insert t.plans ~key:build ~value:module_plan in
    Ok (Work_result.Complete [])

let execute = fun t registry (build: Goal.build_package) ->
  match find t build with
  | Some _ -> Ok (Work_result.Complete [])
  | None ->
      let* depset = Package_planning.depset t.package_planning build in
      let* input = Package_planning.resolve ~depset t.package_planning build in
      match load_cached_plan t input with
      | Error _ as error -> error
      | Ok (Some _) -> Ok (Work_result.Complete [])
      | Ok None ->
          let source_dependencies = source_dependency_requests t build in
          let* source_dependencies = source_dependencies in
          let package = input.package in
          let tasks =
            package_source_tasks t ~package ~toolchain:input.toolchain ~build_ctx:input.build_ctx
          in
          let missing = Source_analyzer.missing t.source_analyzer ~package:package.name tasks in
          if List.is_empty missing then
            plan t registry build
          else
            Ok (Work_result.RequeueWithDependencies source_dependencies)
