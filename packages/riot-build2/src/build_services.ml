open Std
open Std.Result.Syntax

module ConcurrentHashMap = Collections.ConcurrentHashMap
module HashMap = Collections.HashMap
module HashSet = Collections.HashSet
module G = Graph.SimpleGraph

type module_plan = {
  build: Package_work.build_library;
  package: Riot_model.Package.t;
  profile: Riot_model.Profile.t;
  target: Riot_model.Target.t;
  toolchain: Riot_toolchain.t;
  build_ctx: Riot_model.Build_ctx.t;
  action_graph: Riot_planner.Action_graph.t;
  action_nodes: Riot_planner.Action_node.t list;
  sandbox_dir: Path.t;
  package_hash: Crypto.hash;
}

type t = {
  workspace: Riot_model.Workspace.t;
  catalog: Package_catalog.t;
  store: Riot_store.Store.t;
  toolchain_config: Riot_model.Toolchain_config.t;
  session_id: Riot_model.Session_id.t;
  parallelism: int;
  toolchains: (Riot_model.Target.t, Riot_toolchain.t) ConcurrentHashMap.t;
  source_analyses:
    (Source_analysis.key, Riot_planner.Module_graph.source_analysis) ConcurrentHashMap.t;
  module_plans: (Package_work.build_library, module_plan) ConcurrentHashMap.t;
  actions: (Action_execution.ref_, Action_execution.t) ConcurrentHashMap.t;
  action_results: (Action_execution.ref_, Action_execution.result) ConcurrentHashMap.t;
  package_results: (Package_work.build_library, Build_result.package_result) ConcurrentHashMap.t;
  package_actions_registered: (Package_work.build_library, unit) ConcurrentHashMap.t;
}

let create = fun ~workspace ?(parallelism = Thread.available_parallelism) () ->
  {
    workspace;
    catalog = Package_catalog.create workspace;
    store = Riot_store.Store.create ~workspace;
    toolchain_config = Riot_model.Toolchain_config.from_root ~root:workspace.root;
    session_id = Riot_model.Session_id.make ();
    parallelism = Int.max 1 parallelism;
    toolchains = ConcurrentHashMap.with_capacity ~size:16;
    source_analyses = ConcurrentHashMap.with_capacity ~size:512;
    module_plans = ConcurrentHashMap.with_capacity ~size:128;
    actions = ConcurrentHashMap.with_capacity ~size:4_096;
    action_results = ConcurrentHashMap.with_capacity ~size:4_096;
    package_results = ConcurrentHashMap.with_capacity ~size:128;
    package_actions_registered = ConcurrentHashMap.with_capacity ~size:128;
  }

let catalog = fun t -> t.catalog

let package_results = fun t ->
  ConcurrentHashMap.values t.package_results
  |> List.sort
    ~compare:(fun left right ->
      let package_compare =
        Riot_model.Package_name.compare left.Build_result.package right.package
      in
      if package_compare != Order.EQ then
        package_compare
      else
        Riot_model.Target.compare left.target right.target)

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

let apply_package_profile = fun ~(package:Riot_model.Package.t) ~build_ctx profile ->
  let profile = Riot_model.Profile.apply_overrides profile package.compiler.profile_overrides in
  let target_platform = Riot_model.Build_ctx.target_platform_name build_ctx in
  List.find
    package.compiler.target_overrides
    ~fn:(fun (target, _) -> String.equal target target_platform)
  |> Option.and_then
    ~fn:(fun (_, (target_override: Riot_model.Package.target_override)) ->
      target_override.profile_override)
  |> Option.map ~fn:(fun override -> Riot_model.Profile.apply_override profile override)
  |> Option.unwrap_or ~default:profile

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

let source_key_for_task = fun package task ->
  Source_analysis.key_of_task
    ~package:package.Riot_model.Package.name
    task

let action_ref_key = fun (action: Action_execution.t) -> Work_node.ActionExecutionKey action.ref_

let action_dependency_key = fun action_ref -> Work_node.ActionExecutionKey action_ref

let store_error = fun ?package reason -> Error.StoreFailed { package; reason }

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

let ensure_toolchain = fun t (toolchain: Toolchain_ready.t) ->
  match ConcurrentHashMap.get t.toolchains ~key:toolchain.target with
  | Some _ -> Ok (Executor.Complete [])
  | None ->
      match Riot_toolchain.init_for_target ~config:t.toolchain_config ~target:toolchain.target with
      | Ok ready ->
          ignore (ConcurrentHashMap.insert t.toolchains ~key:toolchain.target ~value:ready);
          Ok (Executor.Complete [])
      | Error reason -> Error (Error.ToolchainFailed { target = toolchain.target; reason })

let execute_source_analysis = fun t (source: Source_analysis.t) ->
  match Riot_planner.Module_graph.analyze_source source.task with
  | Ok analysis ->
      ignore (ConcurrentHashMap.insert t.source_analyses ~key:source.key ~value:analysis);
      Ok (Executor.Complete [])
  | Error error ->
      Error (Error.SourceAnalysisFailed {
        source = source.task.task_display_path;
        reason = Riot_planner.Planning_error.to_string error;
      })

let build_ctx = fun t ~(profile:Riot_model.Profile.t) ~target ->
  let host = Riot_toolchain.get_host_triple () in
  let compilation_mode =
    if Riot_model.Target.equal host target then
      Riot_model.Build_ctx.HostOnly
    else
      Riot_model.Build_ctx.Cross {
        target;
        sysroot = None;
        bin_dir = None;
        bin_prefix = "";
      }
  in
  Riot_model.Build_ctx.make
    ~session_id:t.session_id
    ~profile
    ~compilation_mode
    ~parallelism:t.parallelism
    ()

let module_plan_hash = fun
  ~(package:Riot_model.Package.t) ~profile ~build_ctx ~toolchain action_nodes ->
  let hasher = Crypto.Sha256.create () in
  Crypto.Sha256.write hasher "riot-build2-package-plan:v1";
  Riot_model.Package.hash hasher package;
  Riot_model.Profile.hash hasher profile;
  Riot_model.Build_ctx.hash hasher build_ctx;
  Crypto.Sha256.write_hash hasher (Riot_toolchain.hash toolchain);
  List.for_each
    action_nodes
    ~fn:(fun action -> Crypto.Sha256.write_hash hasher (Riot_planner.Action_node.get_hash action));
  Crypto.Sha256.finish hasher

let analyze_sources_from_cache = fun t package ~on_source_analyzed tasks ->
  let source_count = List.length tasks in
  tasks
  |> List.enumerate
  |> List.map
    ~fn:(fun (index, task) ->
      let key = source_key_for_task package task in
      let analysis =
        match ConcurrentHashMap.get t.source_analyses ~key with
        | Some cached -> Ok { cached with Riot_planner.Module_graph.analysis_task = task }
        | None -> Riot_planner.Module_graph.analyze_source task
      in
      match analysis with
      | Ok analysis ->
          on_source_analyzed
            Riot_planner.Module_graph.{
              source = task.task_display_path;
              source_index = Int.succ index;
              source_count;
            };
          Ok analysis
      | Error error -> Error error)

let plan_module_graph = fun t registry (build: Package_work.build_library) ->
  let* package =
    Package_catalog.realize t.catalog ~intent:Riot_model.Package.Runtime build.Package_work.package
  in
  match ConcurrentHashMap.get t.toolchains ~key:build.target with
  | None ->
      Error (Error.ToolchainFailed {
        target = build.target;
        reason = "toolchain was not ready before module planning";
      })
  | Some toolchain ->
      let base_ctx = build_ctx t ~profile:build.profile ~target:build.target in
      let profile = apply_package_profile ~package ~build_ctx:base_ctx build.profile in
      let build_ctx = build_ctx t ~profile ~target:build.target in
      let tasks = package_source_tasks t ~package ~toolchain ~build_ctx in
      let missing =
        List.filter_map
          tasks
          ~fn:(fun task ->
            let key = source_key_for_task package task in
            match ConcurrentHashMap.get t.source_analyses ~key with
            | Some _ -> None
            | None -> Some (Source_analysis.make ~package:package.name ~task))
      in
      if not (List.is_empty missing) then
        Ok (
          Executor.RequeueWithDependencies (
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
            ~scope:Riot_model.Package.Normal
            ~intent:Riot_model.Package.Runtime
            package
        in
        let input =
          Riot_planner.Module_planner.{
            package;
            profile;
            ctx = build_ctx;
            toolchain;
            workspace = t.workspace;
            source_groups = source_groups package;
            depset = [];
            dependency_packages;
            store = t.store;
            on_source_analyzed = (fun _ -> ());
          }
        in
        match Riot_planner.Module_planner.plan_node
          ~analyze_sources:(analyze_sources_from_cache t package)
          input with
        | Error error ->
            Error (Error.ModulePlanningFailed {
              package = package.name;
              reason = Riot_planner.Planning_error.to_string error;
            })
        | Ok plan ->
            let action_nodes = Riot_planner.Action_graph.nodes plan.action_graph in
            let sandbox_dir =
              Path.(Riot_model.Riot_dirs.sandbox_dir_in_workspace
                ~workspace:t.workspace
                ~profile:profile.name
                ~target:build.target
              / Path.v (Riot_model.Package_name.to_string package.name)
              / Path.v
                (Crypto.Digest.hex
                  (module_plan_hash ~package ~profile ~build_ctx ~toolchain action_nodes)))
            in
            let* sandbox_dir = absolute_path sandbox_dir in
            let package_hash =
              module_plan_hash ~package ~profile ~build_ctx ~toolchain action_nodes
            in
            let module_plan = {
              build;
              package;
              profile;
              target = build.target;
              toolchain;
              build_ctx;
              action_graph = plan.action_graph;
              action_nodes;
              sandbox_dir;
              package_hash;
            }
            in
            ignore (ConcurrentHashMap.insert t.module_plans ~key:build ~value:module_plan);
            Ok (Executor.Complete [])

let execute_module_plan = fun t registry (build: Package_work.build_library) ->
  match ConcurrentHashMap.get t.module_plans ~key:build with
  | Some _ -> Ok (Executor.Complete [])
  | None ->
      match ConcurrentHashMap.get t.toolchains ~key:build.target with
      | Some _ -> plan_module_graph t registry build
      | None ->
          Ok (Executor.RequeueWithDependencies [
            Work_node.ToolchainReadyKey { target = build.target };
          ])

let register_action_nodes = fun t registry (plan: module_plan) ->
  let refs_by_id = HashMap.create () in
  List.for_each
    plan.action_nodes
    ~fn:(fun action ->
      let ref_ =
        Action_execution.ref_of_action
          ~package:plan.package.name
          ~profile:plan.profile
          ~target:plan.target
          action
      in
      ignore (HashMap.insert refs_by_id ~key:(Riot_planner.Action_node.id action) ~value:ref_));
  List.map
    plan.action_nodes
    ~fn:(fun action ->
      let ref_ =
        HashMap.get refs_by_id ~key:(Riot_planner.Action_node.id action)
        |> Option.expect ~msg:"action ref should have been registered"
      in
      let dependencies =
        Riot_planner.Action_node.deps action
        |> List.filter_map ~fn:(fun dep_id -> HashMap.get refs_by_id ~key:dep_id)
      in
      let action_execution = {
        Action_execution.ref_;
        action;
        dependencies;
        sandbox_dir = plan.sandbox_dir;
      }
      in
      ignore (ConcurrentHashMap.insert t.actions ~key:ref_ ~value:action_execution);
      Work_registry.intern_action_execution registry action_execution
      |> ignore;
      ref_)

let action_result_failed = fun result ->
  match result.Action_execution.status with
  | Action_execution.Failed reason -> Some reason
  | Cached _
  | Executed _ -> None

let action_result_artifact = fun t ref_ ->
  ConcurrentHashMap.get t.action_results ~key:ref_
  |> Option.and_then ~fn:Action_execution.artifact

let compute_export_entries = fun t (plan: module_plan) ->
  plan.action_nodes
  |> List.flat_map
    ~fn:(fun node ->
      let is_package_export =
        List.any
          (Riot_planner.Action_node.value node).actions
          ~fn:(fun __tmp1 ->
            match __tmp1 with
            | Riot_planner.Action.CreateLibrary _
            | Riot_planner.Action.CreateExecutable _
            | Riot_planner.Action.CreateSharedLibrary _ -> true
            | CompileInterface _
            | CompileImplementation _
            | GenerateInterface _
            | CompileC _
            | CopyFile _
            | WriteFile _
            | BuildForeignDependency _ -> false)
      in
      if not is_package_export then
        []
      else
        let ref_ =
          Action_execution.ref_of_action
            ~package:plan.package.name
            ~profile:plan.profile
            ~target:plan.target
            node
        in
        match action_result_artifact t ref_ with
        | None -> []
        | Some artifact ->
            let action_hash = Crypto.Digest.hex artifact.Riot_store.Artifact.input_hash in
            List.map
              (Riot_planner.Action_node.value node).outs
              ~fn:(fun out_path ->
                Riot_store.Store.{ name = Path.basename out_path; path = out_path; action_hash }))

let collect_package_outputs = fun (plan: module_plan) ->
  let seen = HashSet.create () in
  plan.action_nodes
  |> List.flat_map ~fn:(fun node -> (Riot_planner.Action_node.value node).outs)
  |> List.filter_map
    ~fn:(fun out ->
      let abs = Path.join plan.sandbox_dir out in
      if HashSet.insert seen ~value:(Path.to_string abs) then
        Some abs
      else
        None)

let finalize_package = fun t (plan: module_plan) ->
  let failed =
    plan.action_nodes
    |> List.filter_map
      ~fn:(fun node ->
        let ref_ =
          Action_execution.ref_of_action
            ~package:plan.package.name
            ~profile:plan.profile
            ~target:plan.target
            node
        in
        ConcurrentHashMap.get t.action_results ~key:ref_
        |> Option.and_then ~fn:action_result_failed)
  in
  match failed with
  | reason :: _ -> Error (Error.ActionExecutionFailed { package = plan.package.name; reason })
  | [] ->
      let exports = compute_export_entries t plan in
      let target_dir =
        Path.(Riot_model.Riot_dirs.out_dir_in_workspace
          ~workspace:t.workspace
          ~profile:plan.profile.name
          ~target:plan.target
        / Path.v (Riot_model.Package_name.to_string plan.package.name))
      in
      match Riot_store.Store.materialize_package_exports t.store ~exports ~target_dir with
      | Error error ->
          Error (store_error ~package:plan.package.name (Riot_store.Store.error_message error))
      | Ok () ->
          let outputs = collect_package_outputs plan in
          let warnings =
            plan.action_nodes
            |> List.filter_map
              ~fn:(fun node ->
                let ref_ =
                  Action_execution.ref_of_action
                    ~package:plan.package.name
                    ~profile:plan.profile
                    ~target:plan.target
                    node
                in
                ConcurrentHashMap.get t.action_results ~key:ref_)
            |> List.flat_map ~fn:(fun result -> result.Action_execution.ocamlc_warnings)
          in
          match Riot_store.Store.save_package
            t.store
            ~package:(Riot_model.Package_name.to_string plan.package.name)
            ~ocamlc_warnings:warnings
            ~exports
            ~input_hash:plan.package_hash
            ~sandbox_dir:plan.sandbox_dir
            ~outs:outputs with
          | Error error ->
              Error (store_error ~package:plan.package.name (Riot_store.Store.error_message error))
          | Ok artifact ->
              let result =
                Build_result.{
                  package = plan.package.name;
                  profile = plan.profile;
                  target = plan.target;
                  status = Built artifact;
                  ocamlc_warnings = warnings;
                }
              in
              ignore (ConcurrentHashMap.insert t.package_results ~key:plan.build ~value:result);
              Ok (Executor.Complete [])

let execute_package_finalize = fun t registry (build: Package_work.build_library) ->
  match ConcurrentHashMap.get t.package_results ~key:build with
  | Some _ -> Ok (Executor.Complete [])
  | None ->
      match ConcurrentHashMap.get t.module_plans ~key:build with
      | None -> Ok (Executor.RequeueWithDependencies [ Work_node.ModulePlanKey build ])
      | Some plan ->
          let registered =
            ConcurrentHashMap.compute
              t.package_actions_registered
              ~key:build
              ~fn:(fun current ->
                match current with
                | Some () -> ConcurrentHashMap.Abort true
                | None -> ConcurrentHashMap.Insert ((), false))
          in
          if not registered then
            let action_refs = register_action_nodes t registry plan in
            Ok (Executor.RequeueWithDependencies (List.map action_refs ~fn:action_dependency_key))
          else
            let missing =
              plan.action_nodes
              |> List.filter_map
                ~fn:(fun node ->
                  let ref_ =
                    Action_execution.ref_of_action
                      ~package:plan.package.name
                      ~profile:plan.profile
                      ~target:plan.target
                      node
                  in
                  match ConcurrentHashMap.get t.action_results ~key:ref_ with
                  | Some _ -> None
                  | None -> Some ref_)
            in
            if not (List.is_empty missing) then
              Ok (Executor.RequeueWithDependencies (List.map missing ~fn:action_dependency_key))
            else
              finalize_package t plan

let compute_action_input_hash = fun ~planned_hash ~dependency_output_hashes ->
  let hasher = Crypto.Sha256.create () in
  Crypto.Sha256.write hasher "riot-build2-action-input:v1";
  Crypto.Sha256.write_hash hasher planned_hash;
  List.for_each dependency_output_hashes ~fn:(Crypto.Sha256.write_hash hasher);
  Crypto.Sha256.finish hasher

let resolve_include_paths = fun sandbox_dir includes ->
  List.map
    includes
    ~fn:(fun inc ->
      let inc_str = Path.to_string inc in
      if Path.is_absolute inc || String.starts_with ~prefix:"+" inc_str then
        inc
      else
        Path.join sandbox_dir inc)

let make_flags_absolute = fun sandbox_dir flags ->
  List.map
    flags
    ~fn:(fun flag ->
      match flag with
      | Riot_toolchain.Ocamlc.Impl path -> Riot_toolchain.Ocamlc.Impl (Path.join sandbox_dir path)
      | other -> other)

let ocamlc_success = fun message -> Riot_toolchain.Ocamlc.Success { message; diagnostics = [] }

let ocamlc_failed = fun message -> Riot_toolchain.Ocamlc.Failed { message; diagnostics = [] }

let ensure_parent_dir = fun path ->
  match Path.parent path with
  | Some dir -> Fs.create_dir_all dir
  | None -> Ok ()

let run_action = fun ?c_compiler ocamlc sandbox_dir action ->
  match action with
  | Riot_planner.Action.CompileInterface {
      source;
      outputs = output :: _;
      includes;
      flags;
    } ->
      Riot_toolchain.Ocamlc.compile_interface
        ocamlc
        ~cwd:sandbox_dir
        ~includes:(resolve_include_paths sandbox_dir includes)
        ~flags:(make_flags_absolute sandbox_dir flags)
        ~output:(Path.join sandbox_dir output)
        (Path.join sandbox_dir source)
      |> Riot_toolchain.Ocamlc.run
  | CompileImplementation {
      source;
      outputs = output :: _;
      includes;
      flags;
    } ->
      Riot_toolchain.Ocamlc.compile_impl
        ocamlc
        ~cwd:sandbox_dir
        ~includes:(resolve_include_paths sandbox_dir includes)
        ~flags:(make_flags_absolute sandbox_dir flags)
        ~output:(Path.join sandbox_dir output)
        (Path.join sandbox_dir source)
      |> Riot_toolchain.Ocamlc.run
  | GenerateInterface {
      source;
      outputs = output :: _;
      includes;
      flags;
    } ->
      Riot_toolchain.Ocamlc.generate_interface
        ocamlc
        ~cwd:sandbox_dir
        ~includes:(resolve_include_paths sandbox_dir includes)
        ~flags:(make_flags_absolute sandbox_dir flags)
        ~output:(Path.join sandbox_dir output)
        (Path.join sandbox_dir source)
      |> Riot_toolchain.Ocamlc.run
  | CompileC { source; outputs = output :: _; ccflags } ->
      let source_dir =
        match Path.parent source with
        | Some dir -> [ Path.join sandbox_dir dir ]
        | None -> [ sandbox_dir ]
      in
      Riot_toolchain.Ocamlc.compile_c
        ocamlc
        ~cwd:sandbox_dir
        ~includes:source_dir
        ?cc:c_compiler
        ~ccflags
        ~output:(Path.join sandbox_dir output)
        (Path.join sandbox_dir source)
      |> Riot_toolchain.Ocamlc.run
  | CreateLibrary { outputs = output :: _; objects; includes } ->
      Riot_toolchain.Ocamlc.create_library
        ocamlc
        ~cwd:sandbox_dir
        ~includes:(resolve_include_paths sandbox_dir includes)
        ~output:(Path.join sandbox_dir output)
        objects
      |> Riot_toolchain.Ocamlc.run
  | CreateExecutable {
      outputs = output :: _;
      objects;
      libraries;
      includes;
      cclibs;
      ccopt_flags;
      cclib_flags;
    } ->
      Riot_toolchain.Ocamlc.create_executable
        ocamlc
        ~cwd:sandbox_dir
        ~includes:(resolve_include_paths sandbox_dir includes)
        ~libs:libraries
        ?cc:c_compiler
        ~cclibs
        ~ccopt_flags
        ~cclib_flags
        ~output:(Path.join sandbox_dir output)
        (List.map objects ~fn:(Path.join sandbox_dir))
      |> Riot_toolchain.Ocamlc.run
  | CreateSharedLibrary {
      outputs = output :: _;
      objects;
      libraries;
      includes;
      cclibs;
      ccopt_flags;
      cclib_flags;
    } ->
      Riot_toolchain.Ocamlc.create_shared_library
        ocamlc
        ~cwd:sandbox_dir
        ~includes:(resolve_include_paths sandbox_dir includes)
        ~libs:libraries
        ?cc:c_compiler
        ~cclibs
        ~ccopt_flags
        ~cclib_flags
        ~output:(Path.join sandbox_dir output)
        (List.map objects ~fn:(Path.join sandbox_dir))
      |> Riot_toolchain.Ocamlc.run
  | CopyFile { source; destination } ->
      let src =
        if Path.is_absolute source then
          source
        else
          Path.join sandbox_dir source
      in
      let dst = Path.join sandbox_dir destination in
      let _ = ensure_parent_dir dst in
      Fs.copy ~src ~dst
      |> Result.fold
        ~ok:(fun () -> ocamlc_success "copied")
        ~error:(fun error -> ocamlc_failed ("copy failed: " ^ IO.error_message error))
  | WriteFile { destination; content } ->
      let dst = Path.join sandbox_dir destination in
      let _ = ensure_parent_dir dst in
      Fs.write content dst
      |> Result.fold
        ~ok:(fun () -> ocamlc_success "written")
        ~error:(fun error -> ocamlc_failed ("write failed: " ^ IO.error_message error))
  | BuildForeignDependency { name; _ } ->
      ocamlc_failed ("foreign dependency builds are not supported yet: " ^ name)
  | CompileInterface { outputs = []; _ }
  | CompileImplementation { outputs = []; _ }
  | GenerateInterface { outputs = []; _ }
  | CompileC { outputs = []; _ }
  | CreateLibrary { outputs = []; _ }
  | CreateExecutable { outputs = []; _ }
  | CreateSharedLibrary { outputs = []; _ } -> ocamlc_failed "action has no outputs"

let resolve_source_for_copy = fun ~(package:Riot_model.Package.t) source ->
  if Path.is_absolute source then
    source
  else
    Path.join package.path source

let copy_sources = fun ~(package:Riot_model.Package.t) ~sandbox_dir sources ->
  let rec loop = fun __tmp1 ->
    match __tmp1 with
    | [] -> Ok ()
    | source :: rest ->
        let src = resolve_source_for_copy ~package source in
        let dst = Path.join sandbox_dir source in
        let* () =
          match Path.parent dst with
          | Some dir ->
              Fs.create_dir_all dir
              |> Result.map_err ~fn:IO.error_message
          | None -> Ok ()
        in
        let* () =
          Fs.copy ~src ~dst
          |> Result.map_err ~fn:IO.error_message
        in
        loop rest
  in
  loop sources

let verify_outputs = fun outputs ->
  let missing =
    List.filter
      outputs
      ~fn:(fun output ->
        match Fs.exists output with
        | Ok true -> false
        | Ok false
        | Error _ -> true)
  in
  if List.is_empty missing then
    Ok ()
  else
    Error missing

let execute_actions = fun t (action: Action_execution.t) toolchain action_input_hash ->
  let node = action.action in
  let spec = Riot_planner.Action_node.value node in
  let package = spec.package in
  let sandbox_dir = action.sandbox_dir in
  let _ = Fs.create_dir_all sandbox_dir in
  let* () =
    copy_sources ~package ~sandbox_dir spec.srcs
    |> Result.map_err
      ~fn:(fun reason -> Error.ActionExecutionFailed { package = package.name; reason })
  in
  let ocamlc = Riot_toolchain.ocamlc toolchain in
  let c_compiler = Riot_toolchain.c_compiler toolchain in
  let rec run_all warnings = fun __tmp1 ->
    match __tmp1 with
    | [] -> Ok warnings
    | action :: rest ->
        match run_action ?c_compiler ocamlc sandbox_dir action with
        | Riot_toolchain.Ocamlc.Success _ as result ->
            run_all (warnings @ Riot_toolchain.Ocamlc.get_ocamlc_warnings result) rest
        | Riot_toolchain.Ocamlc.Failed _ as result ->
            Error (Error.ActionExecutionFailed {
              package = package.name;
              reason = Riot_toolchain.Ocamlc.get_output result;
            })
  in
  let* warnings = run_all [] spec.actions in
  let abs_outputs = List.map spec.outs ~fn:(Path.join sandbox_dir) in
  let* () =
    verify_outputs abs_outputs
    |> Result.map_err
      ~fn:(fun missing -> Error.ActionOutputsNotCreated { package = package.name; missing })
  in
  match Riot_store.Store.save_action
    t.store
    ~package:(Riot_model.Package_name.to_string package.name)
    ~ocamlc_warnings:warnings
    ~input_hash:action_input_hash
    ~sandbox_dir
    ~outs:abs_outputs with
  | Error error -> Error (store_error ~package:package.name (Riot_store.Store.error_message error))
  | Ok saved_artifact ->
      let result = {
        Action_execution.ref_ = action.ref_;
        status = Action_execution.Executed saved_artifact;
        ocamlc_warnings = warnings;
      }
      in
      ignore (ConcurrentHashMap.insert t.action_results ~key:action.ref_ ~value:result);
      Ok (Executor.Complete [])

let promote_cached_action = fun t (action: Action_execution.t) (artifact: Riot_store.Artifact.t) ->
  match Riot_store.Store.promote_action t.store artifact.input_hash ~target_dir:action.sandbox_dir with
  | Error error ->
      Error (store_error ~package:action.ref_.package (Riot_store.Store.error_message error))
  | Ok () ->
      let result = {
        Action_execution.ref_ = action.ref_;
        status = Action_execution.Cached artifact;
        ocamlc_warnings = artifact.ocamlc_warnings;
      }
      in
      ignore (ConcurrentHashMap.insert t.action_results ~key:action.ref_ ~value:result);
      Ok (Executor.Complete [])

let execute_action = fun t (action: Action_execution.t) ->
  let missing =
    action.dependencies
    |> List.filter
      ~fn:(fun ref_ -> Option.is_none (ConcurrentHashMap.get t.action_results ~key:ref_))
  in
  if not (List.is_empty missing) then
    Ok (Executor.RequeueWithDependencies (List.map missing ~fn:action_dependency_key))
  else
    let failed =
      action.dependencies
      |> List.filter_map
        ~fn:(fun ref_ ->
          ConcurrentHashMap.get t.action_results ~key:ref_
          |> Option.and_then ~fn:action_result_failed)
    in
    match failed with
    | reason :: _ ->
        let package = action.ref_.package in
        let result = {
          Action_execution.ref_ = action.ref_;
          status = Action_execution.Failed reason;
          ocamlc_warnings = [];
        }
        in
        ignore (ConcurrentHashMap.insert t.action_results ~key:action.ref_ ~value:result);
        Error (Error.ActionExecutionFailed { package; reason })
    | [] ->
        match ConcurrentHashMap.get t.toolchains ~key:action.ref_.target with
        | None ->
            Error (Error.ToolchainFailed {
              target = action.ref_.target;
              reason = "toolchain was not ready before action execution";
            })
        | Some toolchain ->
            let dependency_output_hashes =
              action.dependencies
              |> List.filter_map
                ~fn:(fun ref_ ->
                  ConcurrentHashMap.get t.action_results ~key:ref_
                  |> Option.and_then ~fn:Action_execution.artifact
                  |> Option.map ~fn:(fun artifact -> artifact.Riot_store.Artifact.output_hash))
            in
            let action_input_hash =
              compute_action_input_hash ~planned_hash:action.ref_.hash ~dependency_output_hashes
            in
            match Riot_store.Store.get_action t.store action_input_hash with
            | Some artifact -> promote_cached_action t action artifact
            | None -> execute_actions t action toolchain action_input_hash

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
  match ConcurrentHashMap.get t.package_results ~key:build with
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
  | ToolchainReady toolchain -> ensure_toolchain t toolchain
  | SourceAnalysis source -> execute_source_analysis t source
  | ModulePlan build -> execute_module_plan t context.Executor.registry build
  | PackageFinalize build -> execute_package_finalize t context.Executor.registry build
  | ActionExecution action -> execute_action t action
