open Std
open Std.Result.Syntax

module ConcurrentHashMap = Collections.ConcurrentHashMap

type t = {
  workspace: Riot_model.Workspace.t;
  catalog: Package_catalog.t;
  store: Riot_store.Store.t;
  session_id: Riot_model.Session_id.t;
  parallelism: int;
  toolchains: Toolchain_service.t;
  source_analyzer: Source_analyzer.t;
  plans: (Package_work.build_library, Module_plan.t) ConcurrentHashMap.t;
}

let create = fun
  ~workspace ~catalog ~store ~session_id ~parallelism ~toolchains ~source_analyzer () ->
  {
    workspace;
    catalog;
    store;
    session_id;
    parallelism;
    toolchains;
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

let plan = fun t registry (build: Package_work.build_library) ->
  let* package =
    Package_catalog.realize t.catalog ~intent:Riot_model.Package.Runtime build.Package_work.package
  in
  match Toolchain_service.find t.toolchains build.target with
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
      let missing = Source_analyzer.missing t.source_analyzer ~package:package.name tasks in
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
          ~analyze_sources:(Source_analyzer.analyze_from_cache t.source_analyzer package)
          input with
        | Error error ->
            Error (Error.ModulePlanningFailed {
              package = package.name;
              reason = Riot_planner.Planning_error.to_string error;
            })
        | Ok planned ->
            let action_nodes = Riot_planner.Action_graph.nodes planned.action_graph in
            let package_hash =
              module_plan_hash ~package ~profile ~build_ctx ~toolchain action_nodes
            in
            let sandbox_dir =
              Path.(Riot_model.Riot_dirs.sandbox_dir_in_workspace
                ~workspace:t.workspace
                ~profile:profile.name
                ~target:build.target
              / Path.v (Riot_model.Package_name.to_string package.name)
              / Path.v (Crypto.Digest.hex package_hash))
            in
            let* sandbox_dir = absolute_path sandbox_dir in
            let module_plan =
              Module_plan.{
                build;
                package;
                profile;
                target = build.target;
                toolchain;
                build_ctx;
                action_graph = planned.action_graph;
                action_nodes;
                sandbox_dir;
                package_hash;
              }
            in
            ignore (ConcurrentHashMap.insert t.plans ~key:build ~value:module_plan);
            Ok (Executor.Complete [])

let execute = fun t registry (build: Package_work.build_library) ->
  match find t build with
  | Some _ -> Ok (Executor.Complete [])
  | None ->
      match Toolchain_service.find t.toolchains build.target with
      | Some _ -> plan t registry build
      | None ->
          Ok (Executor.RequeueWithDependencies [
            Work_node.ToolchainReadyKey { target = build.target };
          ])
