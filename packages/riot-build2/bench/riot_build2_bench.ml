open Std
open Std.Bench
open Riot_build2

let package = fun name ->
  Riot_model.Package_name.from_string name
  |> Result.expect ~msg:("invalid package name: " ^ name)

let target = fun value ->
  Riot_model.Target.from_string value
  |> Result.expect ~msg:("invalid target triple: " ^ value)

let node_id = fun value -> Work_node.Node_id.from_int value

let linux = target "x86_64-unknown-linux-gnu"

let kernel_package = package "kernel"

let build2_package = package "riot-build2"

let available_parallelism = Std.Thread.available_parallelism

let available_parallelism_label = Int.to_string available_parallelism

let executor_workspace =
  Riot_model.Workspace.make
    ~root:(Path.v ".")
    ~target_dir:(Path.v "_build/riot-build2-bench/executor")
    ~packages:[]
    ()

let executor_config = fun parallelism -> Config.make ~workspace:executor_workspace ~parallelism ()

let concrete_execution_mode = fun _node -> Work_node.Concrete

let unexpected_node = fun node ->
  Error (Error.ExecutorInvariantViolated {
    message = "unexpected node in riot-build2 benchmark: "
    ^ Work_node.Node_id.to_string (Work_node.id node);
  })

let sample_goal = fun ?(args = []) name ->
  Goal.RunBinary {
    binary = Goal.BinaryInPackage (package "std", name);
    args;
    profile = Riot_model.Profile.debug;
    target = linux;
  }

let package_names = fun count ->
  let rec loop index acc =
    if Int.equal index count then
      List.reverse acc
    else
      loop (Int.succ index) (package ("pkg" ^ Int.to_string index) :: acc)
  in
  loop 0 []

let actions = fun count ->
  let rec loop index acc =
    if Int.equal index count then
      List.reverse acc
    else
      loop
        (Int.succ index)
        (sample_goal ~args:[ "--index"; Int.to_string index ] ("bench-" ^ Int.to_string index)
        :: acc)
  in
  loop 0 []

let seed = fun () ->
  Work_node.user_intent
    ~id:(node_id 1)
    (User_intent.run ~runnable:(User_intent.ByName "server") ~target:linux ())

let goal_seed = fun id action -> Work_node.goal ~id:(node_id id) action

let goal_request = fun action -> Work_request.existing (Work_node.GoalKey action)

let clone_workspace_with_target = fun (workspace: Riot_model.Workspace.t) ~target_dir ->
  Riot_model.Workspace.make
    ?name:workspace.name
    ~root:workspace.root
    ~packages:workspace.packages
    ~dependencies:workspace.dependencies
    ~dev_dependencies:workspace.dev_dependencies
    ~build_dependencies:workspace.build_dependencies
    ~profile_overrides:workspace.profile_overrides
    ~source_ignore_patterns:workspace.source_ignore_patterns
    ~target_dir
    ()

let load_repo_workspace = fun () ->
  Workspace_loader.load_local ~root:(Path.v ".")
  |> Result.expect ~msg:"failed to load repo workspace for riot-build2 benchmark"

let kernel_goal = fun target ->
  Goal.BuildPackage {
    package = kernel_package;
    scope = Goal.Runtime;
    profile = Riot_model.Profile.debug;
    target;
  }

let kernel_build_intent = fun () ->
  User_intent.build
    ~packages:(User_intent.NamedPackages [ kernel_package ])
    ~targets:(User_intent.ManyTargets [ Riot_model.Target.current ])
    ~profiles:(User_intent.ManyProfiles [ Riot_model.Profile.debug ])
    ()

let source_package_workspace = fun root ->
  let package_name = package "sourcepkg" in
  let package_path = Path.(root / Path.v "sourcepkg") in
  let source = Path.v "src/sourcepkg.ml" in
  Fs.create_dir_all Path.(package_path / Path.v "src")
  |> Result.expect ~msg:"failed to create source package benchmark src dir";
  Fs.write "let value = 1\n" Path.(package_path / source)
  |> Result.expect ~msg:"failed to write source package benchmark source";
  let sources =
    Riot_model.Package.{
      src = [ source ];
      native = [];
      tests = [];
      examples = [];
      bench = [];
    }
  in
  let package =
    Riot_model.Package.make
      ~name:package_name
      ~path:package_path
      ~relative_path:(Path.v "sourcepkg")
      ~library:{ path = source }
      ~sources
      ()
    |> Riot_model.Package_manifest.from_package
  in
  Riot_model.Workspace.make ~root ~target_dir:Path.(root / Path.v "target") ~packages:[ package ] ()

let source_build = fun () ->
  Goal.{
    package = package "sourcepkg";
    scope = Goal.Runtime;
    profile = Riot_model.Profile.debug;
    target = Riot_model.Target.current;
  }

let native_action_package = fun root ->
  Riot_model.Package.make
    ~name:(package "native-action")
    ~path:Path.(root / Path.v "native-action")
    ~relative_path:(Path.v "native-action")
    ()

let native_action_toolchain = fun root target ->
  Riot_toolchain.from_config_for_target
    ~config:(Riot_model.Toolchain_config.from_root ~root)
    ~target

let module_output_paths = fun module_stem -> [
  Path.v (module_stem ^ ".cmt");
  Path.v (module_stem ^ ".cmi");
  Path.v (module_stem ^ ".cmx");
  Path.v (module_stem ^ ".o");
]

let native_compile_library_action = fun ~source_count ~revision:_revision ->
  let rec loop index objects =
    if Int.equal index source_count then
      let library_outputs = [ Path.v "BenchNative.cmxa"; Path.v "BenchNative.a" ] in
      Action.CompileLibrary {
        sources = [];
        objects = List.reverse objects;
        outputs = library_outputs;
        output = Path.v "BenchNative.cmxa";
        includes = [ Path.v "." ];
        flags = [];
      }
    else
      let module_stem = "BenchMod" ^ Int.to_string index in
      let object_ = Path.v (module_stem ^ ".cmx") in
      loop (Int.succ index) (object_ :: objects)
  in
  loop 0 []

let native_compile_sources_action = fun ~source_count ~revision ->
  let rec loop index sources outputs =
    if Int.equal index source_count then
      Action.CompileSources {
        sources = List.reverse sources;
        outputs = List.reverse outputs;
        includes = [ Path.v "." ];
        flags = [];
      }
    else
      let module_stem = "BenchMod" ^ Int.to_string index in
      let content = "let value = " ^ Int.to_string (revision + index) ^ "\n" in
      let source =
        Action.{
          source = Path.v ("generated/" ^ module_stem ^ ".ml");
          staged = Path.v (module_stem ^ ".ml");
          kind = LibraryImplementation;
          content = Some content;
          opens = [];
        }
      in
      let outputs =
        List.fold_left
          (module_output_paths module_stem)
          ~init:outputs
          ~fn:(fun outputs output -> output :: outputs)
      in
      loop (Int.succ index) (source :: sources) outputs
  in
  loop 0 [] []

let native_compile_source_action = fun ~revision ->
  let module_stem = "BenchSource" in
  Action.CompileSource {
    source =
      {
        Action.source = Path.v "generated/BenchSource.ml";
        staged = Path.v "BenchSource.ml";
        kind = Action.LibraryImplementation;
        content = Some ("let value = " ^ Int.to_string revision ^ "\n");
        opens = [];
      };
    outputs = module_output_paths module_stem;
    output = Path.v "BenchSource.cmx";
    includes = [ Path.v "." ];
    flags = [];
  }

let expect_no_failures = fun label summary expected_completed ->
  if
    Int.equal summary.Executor.Summary.failed_count 0
    && Int.equal summary.completed_count expected_completed
  then
    ()
  else
    panic (label ^ " expected completed=" ^ Int.to_string expected_completed ^ " failed=0")

let expect_kernel_package_result = fun result ->
  if Build_result.has_failures result then
    let errors =
      result.Build_result.summary.Executor.Summary.results
      |> List.filter_map
        ~fn:(fun result ->
          match result.Executor.Summary.error with
          | Some error -> Some (Error.message error)
          | None -> None)
    in
    panic
      ("kernel build2 execution benchmark produced executor failures: " ^ String.concat "; " errors);
  match Build_result.package_results result
  |> List.find
    ~fn:(fun package_result ->
      Riot_model.Package_name.equal
        package_result.Build_result.package
        kernel_package) with
  | None -> panic "kernel build2 execution benchmark missed kernel package result"
  | Some package_result ->
      match package_result.Build_result.status with
      | Build_result.Built _
      | Cached _ -> ()
      | Failed error -> panic ("kernel build2 execution benchmark failed: " ^ Error.message error)

let expect_kernel_cached_package_result = fun result ->
  if Build_result.has_failures result then
    let errors =
      result.Build_result.summary.Executor.Summary.results
      |> List.filter_map
        ~fn:(fun result ->
          match result.Executor.Summary.error with
          | Some error -> Some (Error.message error)
          | None -> None)
    in
    panic
      ("kernel build2 warm-cache benchmark produced executor failures: " ^ String.concat "; " errors);
  match Build_result.package_results result
  |> List.find
    ~fn:(fun package_result ->
      Riot_model.Package_name.equal
        package_result.Build_result.package
        kernel_package) with
  | None -> panic "kernel build2 warm-cache benchmark missed kernel package result"
  | Some package_result ->
      match package_result.Build_result.status with
      | Build_result.Cached _ -> ()
      | Built _ -> panic "kernel build2 warm-cache benchmark rebuilt kernel"
      | Failed error -> panic ("kernel build2 warm-cache benchmark failed: " ^ Error.message error)

let make_intent_expand_build_bench = fun ~package_count ->
  let packages = package_names package_count in
  let catalog = Package_catalog.create executor_workspace in
  let intent =
    User_intent.build
      ~packages:(User_intent.NamedPackages packages)
      ~targets:(User_intent.ManyTargets [ linux; Riot_model.Target.current ])
      ()
  in
  fun () ->
    match Intent_planner.expand catalog intent with
    | Ok expanded when Int.equal (List.length expanded) (package_count * 2) -> ()
    | Ok _ -> panic "intent expansion benchmark produced unexpected goal count"
    | Error error -> panic ("intent expansion benchmark failed: " ^ Error.message error)

let make_registry_intern_unique_actions_bench = fun ~count ->
  let goal_list = actions count in
  fun () ->
    let registry = Work_registry.create () in
    List.for_each goal_list ~fn:(fun action -> ignore (Work_registry.intern_goal registry action))

let make_registry_find_goal_hits_bench = fun ~count ->
  let registry = Work_registry.create () in
  let goal_list = actions count in
  List.for_each goal_list ~fn:(fun action -> ignore (Work_registry.intern_goal registry action));
  fun () ->
    List.for_each
      goal_list
      ~fn:(fun action ->
        match Work_registry.find_goal registry action with
        | Some _ -> ()
        | None -> panic "registry find goal hit benchmark missed an action")

let make_executor_independent_actions_bench = fun ~count ~parallelism ->
  let goal_list = actions count in
  fun () ->
    let seeds =
      goal_list
      |> List.enumerate
      |> List.map ~fn:(fun (index, action) -> goal_seed (Int.succ index) action)
    in
    let summary =
      Executor.Runner.run_with_handlers
        ~config:(executor_config parallelism)
        ~execution_mode:concrete_execution_mode
        ~seeds
        ~execute:(fun _context _node -> Ok (Work_result.Complete []))
        ()
    in
    expect_no_failures "independent goals" summary count

let make_executor_spawn_actions_bench = fun ~count ~parallelism ->
  let goal_list = actions count in
  fun () ->
    let summary =
      Executor.Runner.run_with_handlers
        ~config:(executor_config parallelism)
        ~execution_mode:concrete_execution_mode
        ~seeds:[ seed () ]
        ~execute:(fun _context node ->
          match Work_node.kind node with
          | Work_node.UserIntent _ ->
              let spawned = List.map goal_list ~fn:goal_request in
              Ok (Work_result.Complete spawned)
          | Work_node.Goal _ -> Ok (Work_result.Complete [])
          | _ -> unexpected_node node)
        ()
    in
    expect_no_failures "spawn goals" summary (Int.succ count)

let make_executor_dependency_fanout_bench = fun ~count ~parallelism ->
  let goal_list = actions count in
  fun () ->
    let plan_dependencies _registry node =
      match Work_node.kind node with
      | Work_node.UserIntent _ ->
          Ok (List.map goal_list ~fn:goal_request)
      | Work_node.Goal _ -> Ok []
      | _ -> unexpected_node node
    in
    let summary =
      Executor.Runner.run_with_handlers
        ~config:(executor_config parallelism)
        ~execution_mode:concrete_execution_mode
        ~plan_dependencies
        ~seeds:[ seed () ]
        ~execute:(fun _context node ->
          match Work_node.kind node with
          | Work_node.UserIntent _
          | Work_node.Goal _ -> Ok (Work_result.Complete [])
          | _ -> unexpected_node node)
        ()
    in
    expect_no_failures "dependency fanout" summary (Int.succ count)

let make_executor_dependency_chain_bench = fun ~count ~parallelism ->
  let goal_list = actions count in
  let rec next_dependency = fun action actions ->
    match actions with
    | current :: next :: _ when current = action -> Some next
    | _ :: rest -> next_dependency action rest
    | [] -> None
  in
  let intent_dependencies =
    match goal_list with
    | first :: _ -> Ok [ goal_request first ]
    | [] -> Ok []
  in
  let action_dependencies = fun action ->
    match next_dependency action goal_list with
    | Some dependency -> Ok [ goal_request dependency ]
    | None -> Ok []
  in
  fun () ->
    let plan_dependencies _registry node =
      match Work_node.kind node with
      | Work_node.UserIntent _ -> intent_dependencies
      | Work_node.Goal action -> action_dependencies action
      | _ -> unexpected_node node
    in
    let summary =
      Executor.Runner.run_with_handlers
        ~config:(executor_config parallelism)
        ~execution_mode:concrete_execution_mode
        ~plan_dependencies
        ~seeds:[ seed () ]
        ~execute:(fun _context node ->
          match Work_node.kind node with
          | Work_node.UserIntent _
          | Work_node.Goal _ -> Ok (Work_result.Complete [])
          | _ -> unexpected_node node)
        ()
    in
    expect_no_failures "dependency chain" summary (Int.succ count)

let make_workspace_load_bench = fun () ->
  fun () ->
    let workspace = load_repo_workspace () in
    let found_kernel =
      List.any
        workspace.Riot_model.Workspace.packages
        ~fn:(fun (package: Riot_model.Package_manifest.t) ->
          Riot_model.Package_name.equal
            package.name
            kernel_package)
    in
    if found_kernel then
      ()
    else
      panic "workspace load benchmark missed kernel package manifest"

let make_package_catalog_create_bench = fun () ->
  let workspace = load_repo_workspace () in
  fun () ->
    let catalog = Package_catalog.create workspace in
    match Package_catalog.find_manifest catalog kernel_package with
    | Some _ -> ()
    | None -> panic "catalog benchmark missed kernel package"

let make_kernel_runtime_realize_bench = fun () ->
  let workspace = load_repo_workspace () in
  fun () ->
    let catalog = Package_catalog.create workspace in
    match Package_catalog.realize catalog ~intent:Riot_model.Package.Runtime kernel_package with
    | Ok package when Riot_model.Package_name.equal package.name kernel_package ->
        if List.is_empty package.sources.src then
          panic "kernel runtime realization benchmark produced no src files"
        else
          ()
    | Ok _ -> panic "kernel runtime realization benchmark produced unexpected package"
    | Error error -> panic ("kernel runtime realization benchmark failed: " ^ Error.message error)

let make_kernel_runtime_realize_cached_bench = fun () ->
  let workspace = load_repo_workspace () in
  let catalog = Package_catalog.create workspace in
  Package_catalog.begin_execution catalog;
  begin
    match Package_catalog.realize catalog ~intent:Riot_model.Package.Runtime kernel_package with
    | Ok package when Riot_model.Package_name.equal package.name kernel_package -> ()
    | Ok _ -> panic "kernel cached realization benchmark produced unexpected package"
    | Error error ->
        panic ("kernel cached realization benchmark setup failed: " ^ Error.message error)
  end;
  fun () ->
    match Package_catalog.realize catalog ~intent:Riot_model.Package.Runtime kernel_package with
    | Ok package when Riot_model.Package_name.equal package.name kernel_package ->
        if List.is_empty package.sources.src then
          panic "kernel cached realization benchmark produced no src files"
        else
          ()
    | Ok _ -> panic "kernel cached realization benchmark produced unexpected package"
    | Error error ->
        panic ("kernel cached realization benchmark failed: " ^ Error.message error)

let make_kernel_intent_to_goal_bench = fun () ->
  let workspace = load_repo_workspace () in
  let catalog = Package_catalog.create workspace in
  let intent = kernel_build_intent () in
  fun () ->
    match Intent_planner.expand catalog intent with
    | Ok [ Goal.BuildPackage build ] when Riot_model.Package_name.equal build.package kernel_package ->
        ()
    | Ok _ -> panic "kernel intent expansion benchmark produced unexpected goals"
    | Error error -> panic ("kernel intent expansion benchmark failed: " ^ Error.message error)

let make_module_provider_registry_lookup_bench = fun () ->
  let workspace = load_repo_workspace () in
  let catalog = Package_catalog.create workspace in
  let registry = Module_provider_registry.create ~catalog () in
  let build =
    Goal.{
      package = build2_package;
      scope = Goal.Runtime;
      profile = Riot_model.Profile.debug;
      target = Riot_model.Target.current;
    }
  in
  fun () ->
    match Module_provider_registry.providers_for_build registry build with
    | Ok providers when not (List.is_empty providers) -> ()
    | Ok _ -> panic "module provider registry benchmark produced no providers"
    | Error error -> panic ("module provider registry benchmark failed: " ^ Error.message error)

let source_analysis_requests = fun requests ->
  List.filter
    requests
    ~fn:(fun request ->
      match Work_request.key request with
      | Work_node.SourceAnalysisKey _ -> true
      | _ -> false)

let materialize_request = fun registry request ->
  match Work_request.kind request with
  | Some kind -> Work_registry.intern registry ~key:(Work_request.key request) ~make:(fun () -> kind)
  | None -> panic "benchmark request could not be materialized"

let execute_source_analysis_requests = fun services registry requests ->
  List.for_each
    requests
    ~fn:(fun request ->
      let source_node = materialize_request registry request in
      match Build_services.execute_node services registry source_node with
      | Ok (Work_result.Complete []) -> ()
      | Ok (Work_result.Complete _) -> panic "source analysis benchmark produced unexpected requests"
      | Ok (Work_result.RequeueWithDependencies _) ->
          panic "source analysis benchmark requested dependencies"
      | Error error -> panic ("source analysis benchmark failed: " ^ Error.message error))

let execute_module_plan_with_source_summaries = fun services registry node ->
  let source_keys =
    match Build_services.plan_dependencies services registry node with
    | Ok keys -> source_analysis_requests keys
    | Error error -> panic ("module plan benchmark dependency planning failed: " ^ Error.message error)
  in
  if List.is_empty source_keys then
    panic "module plan benchmark planned no source analysis dependencies"
  else
    execute_source_analysis_requests services registry source_keys;
  match Build_services.execute_node services registry node with
  | Ok (Work_result.Complete []) -> ()
  | Ok (Work_result.Complete _) -> panic "module plan benchmark produced unexpected requests"
  | Ok (Work_result.RequeueWithDependencies _) ->
      panic "module plan benchmark requested dependencies after source summaries"
  | Error error -> panic ("module plan benchmark failed: " ^ Error.message error)

let execute_module_plan_to_cache = fun workspace ->
  let config = Config.make ~workspace ~parallelism:available_parallelism () in
  let services = Build_services.create ~config () in
  let registry = Work_registry.create () in
  let node = Work_node.module_plan ~id:(node_id 1) (source_build ()) in
  execute_module_plan_with_source_summaries services registry node

let make_module_plan_cache_hit_bench = fun () ->
  let workspace =
    source_package_workspace
      Path.(Path.v "_bench" / Path.v ("module-plan-cache-" ^ UUID.to_string (UUID.v4 ())))
  in
  execute_module_plan_to_cache workspace;
  fun () ->
    let config = Config.make ~workspace ~parallelism:available_parallelism () in
    let services = Build_services.create ~config () in
    let registry = Work_registry.create () in
    let node = Work_node.module_plan ~id:(node_id 1) (source_build ()) in
    execute_module_plan_with_source_summaries services registry node

let make_module_dependencies_from_cached_module_plan_bench = fun () ->
  let workspace =
    source_package_workspace
      Path.(Path.v "_bench" / Path.v ("module-dependencies-cache-" ^ UUID.to_string (UUID.v4 ())))
  in
  execute_module_plan_to_cache workspace;
  fun () ->
    let config = Config.make ~workspace ~parallelism:available_parallelism () in
    let services = Build_services.create ~config () in
    let registry = Work_registry.create () in
    let build = source_build () in
    let node = Work_node.module_dependencies ~id:(node_id 1) build in
    execute_module_plan_with_source_summaries services registry node

let make_native_compile_library_action_hash_bench = fun ~source_count ->
  let root = Path.v "." in
  let package = native_action_package root in
  let target = Riot_model.Target.current in
  let toolchain = native_action_toolchain root target in
  let action = native_compile_library_action ~source_count ~revision:0 in
  fun () -> ignore (Action.hash ~package ~toolchain action)

let make_native_compile_sources_action_hash_bench = fun ~source_count ->
  let root = Path.v "." in
  let package = native_action_package root in
  let target = Riot_model.Target.current in
  let toolchain = native_action_toolchain root target in
  let action = native_compile_sources_action ~source_count ~revision:0 in
  fun () -> ignore (Action.hash ~package ~toolchain action)

let make_native_compile_source_action_hash_bench = fun () ->
  let root = Path.v "." in
  let package = native_action_package root in
  let target = Riot_model.Target.current in
  let toolchain = native_action_toolchain root target in
  let action = native_compile_source_action ~revision:0 in
  fun () -> ignore (Action.hash ~package ~toolchain action)

let make_native_compile_library_action_json_roundtrip_bench = fun ~source_count ->
  let action = native_compile_library_action ~source_count ~revision:0 in
  fun () ->
    match Serde_json.to_string Action.serialize action with
    | Error error ->
        panic ("compile-library action serialization failed: " ^ Serde.Error.to_string error)
    | Ok encoded ->
        match Serde_json.from_string Action.deserialize encoded with
        | Ok decoded when decoded = action -> ()
        | Ok _ -> panic "compile-library action json roundtrip changed the action"
        | Error error ->
            panic ("compile-library action deserialization failed: " ^ Serde.Error.to_string error)

let make_native_compile_sources_action_json_roundtrip_bench = fun ~source_count ->
  let action = native_compile_sources_action ~source_count ~revision:0 in
  fun () ->
    match Serde_json.to_string Action.serialize action with
    | Error error ->
        panic ("compile-sources action serialization failed: " ^ Serde.Error.to_string error)
    | Ok encoded ->
        match Serde_json.from_string Action.deserialize encoded with
        | Ok decoded when decoded = action -> ()
        | Ok _ -> panic "compile-sources action json roundtrip changed the action"
        | Error error ->
            panic ("compile-sources action deserialization failed: " ^ Serde.Error.to_string error)

let make_native_compile_source_action_json_roundtrip_bench = fun () ->
  let action = native_compile_source_action ~revision:0 in
  fun () ->
    match Serde_json.to_string Action.serialize action with
    | Error error ->
        panic ("compile-source action serialization failed: " ^ Serde.Error.to_string error)
    | Ok encoded ->
        match Serde_json.from_string Action.deserialize encoded with
        | Ok decoded when decoded = action -> ()
        | Ok _ -> panic "compile-source action json roundtrip changed the action"
        | Error error ->
            panic ("compile-source action deserialization failed: " ^ Serde.Error.to_string error)

let make_native_compile_sources_execution = fun ~root ~source_count ~revision ~toolchain ->
  let target = Riot_model.Target.current in
  let package = native_action_package root in
  Action_execution.make
    ~package
    ~profile:Riot_model.Profile.debug
    ~target
    ~toolchain
    ~action:(native_compile_sources_action ~source_count ~revision)
    ~dependencies:[]
    ~sandbox_dir:Path.(root / Path.v "sandbox" / Path.v (Int.to_string revision))

let expect_native_compile_sources_executed = fun executor action ->
  match Action_executor.execute executor action with
  | Ok (Work_result.Complete []) ->
      let result = Action_executor.find_result executor action.Action_execution.ref_ in
      (
        match result with
        | Some { Action_execution.status = Action_execution.Executed _; _ } -> ()
        | Some _ -> panic "native compile-sources action benchmark expected executed result"
        | None -> panic "native compile-sources action benchmark missed action result"
      )
  | Ok _ -> panic "native compile-sources action benchmark requested dependencies"
  | Error error -> panic ("native compile-sources action benchmark failed: " ^ Error.message error)

let expect_native_compile_sources_cached = fun executor action ->
  match Action_executor.execute executor action with
  | Ok (Work_result.Complete []) ->
      let result = Action_executor.find_result executor action.Action_execution.ref_ in
      (
        match result with
        | Some { Action_execution.status = Action_execution.Cached _; _ } -> ()
        | Some _ -> panic "native compile-sources cache benchmark expected cached result"
        | None -> panic "native compile-sources cache benchmark missed action result"
      )
  | Ok _ -> panic "native compile-sources cache benchmark requested dependencies"
  | Error error -> panic ("native compile-sources cache benchmark failed: " ^ Error.message error)

type native_compile_sources_cold_state = {
  cold_root: Path.t;
  cold_toolchain: Riot_toolchain.t;
  cold_executor: Action_executor.t;
}

let make_native_compile_sources_cold_state = fun () ->
  let root =
    Path.(Path.v "_bench" / Path.v ("compile-sources-cold-" ^ UUID.to_string (UUID.v4 ())))
  in
  Fs.create_dir_all root
  |> Result.expect ~msg:"failed to create native compile-sources benchmark root";
  let workspace =
    Riot_model.Workspace.make ~root ~target_dir:Path.(root / Path.v "target") ~packages:[] ()
  in
  let store = Riot_store.Store.create ~workspace in
  let toolchains = Toolchain_service.create ~root () in
  let target = Riot_model.Target.current in
  Toolchain_service.ensure toolchains (Toolchain_ready.make ~target)
  |> Result.expect ~msg:"failed to ready native compile-sources benchmark toolchain";
  let toolchain =
    match Toolchain_service.find toolchains target with
    | Some toolchain -> toolchain
    | None -> panic "native compile-sources benchmark toolchain was not registered"
  in
  {
    cold_root = root;
    cold_toolchain = toolchain;
    cold_executor = Action_executor.create ~store ~toolchains ();
  }

type native_compile_sources_cached_state = {
  cached_store: Riot_store.Store.t;
  cached_toolchains: Toolchain_service.t;
  cached_action: Action_execution.t;
}

let make_native_compile_sources_cached_state = fun ~source_count ->
  let root =
    Path.(Path.v "_bench" / Path.v ("compile-sources-cached-" ^ UUID.to_string (UUID.v4 ())))
  in
  Fs.create_dir_all root
  |> Result.expect ~msg:"failed to create native compile-sources cache benchmark root";
  let workspace =
    Riot_model.Workspace.make ~root ~target_dir:Path.(root / Path.v "target") ~packages:[] ()
  in
  let store = Riot_store.Store.create ~workspace in
  let toolchains = Toolchain_service.create ~root () in
  let target = Riot_model.Target.current in
  Toolchain_service.ensure toolchains (Toolchain_ready.make ~target)
  |> Result.expect ~msg:"failed to ready native compile-sources cache benchmark toolchain";
  let toolchain =
    match Toolchain_service.find toolchains target with
    | Some toolchain -> toolchain
    | None -> panic "native compile-sources cache benchmark toolchain was not registered"
  in
  let action = make_native_compile_sources_execution ~root ~source_count ~revision:0 ~toolchain in
  let seed_executor = Action_executor.create ~store ~toolchains () in
  expect_native_compile_sources_executed seed_executor action;
  { cached_store = store; cached_toolchains = toolchains; cached_action = action }

let make_native_compile_sources_cold_execution_bench = fun ~source_count ->
  let state = ref None in
  let state_or_create = fun () ->
    match !state with
    | Some state -> state
    | None ->
        let created = make_native_compile_sources_cold_state () in
        state := Some created;
        created
  in
  let counter = ref 0 in
  fun () ->
    let state = state_or_create () in
    let revision = !counter in
    counter := revision + 1;
    let action =
      make_native_compile_sources_execution
        ~root:state.cold_root
        ~source_count
        ~revision
        ~toolchain:state.cold_toolchain
    in
    expect_native_compile_sources_executed state.cold_executor action

let make_native_compile_sources_cached_execution_bench = fun ~source_count ->
  let state = ref None in
  let state_or_create = fun () ->
    match !state with
    | Some state -> state
    | None ->
        let created = make_native_compile_sources_cached_state ~source_count in
        state := Some created;
        created
  in
  fun () ->
    let state = state_or_create () in
    let executor =
      Action_executor.create ~store:state.cached_store ~toolchains:state.cached_toolchains ()
    in
    expect_native_compile_sources_cached executor state.cached_action

let kernel_cold_target_dir = fun () ->
  Path.(Path.v "_bench" / Path.v ("kernel-" ^ UUID.to_string (UUID.v4 ())))

let kernel_warm_cache_target_dir = fun () ->
  Path.(Path.v "_build"
  / Path.v "riot-build2-bench"
  / Path.v ("kernel-warm-cache-" ^ UUID.to_string (UUID.v4 ())))

let rec copy_tree = fun ~src ~dst ->
  Fs.create_dir_all dst
  |> Result.expect ~msg:"failed to create benchmark copy destination";
  let entries =
    Fs.read_dir src
    |> Result.expect ~msg:"failed to read benchmark copy source"
  in
  let rec loop () =
    match Iter.MutIterator.next entries with
    | None -> ()
    | Some entry ->
        let entry_name = Path.v (Path.basename entry) in
        let src_entry =
          if Path.is_absolute entry then
            entry
          else
            Path.(src / entry)
        in
        let dst_entry = Path.(dst / entry_name) in
        let is_dir =
          Fs.is_dir src_entry
          |> Result.expect ~msg:"failed to inspect benchmark copy source"
        in
        if is_dir then
          copy_tree ~src:src_entry ~dst:dst_entry
        else
          Fs.copy ~src:src_entry ~dst:dst_entry
          |> Result.expect ~msg:"failed to copy benchmark source file";
        loop ()
  in
  loop ()

let isolated_kernel_workspace = fun root ->
  let workspace = load_repo_workspace () in
  let manifest =
    List.find
      workspace.packages
      ~fn:(fun (manifest: Riot_model.Package_manifest.t) ->
        Riot_model.Package_name.equal
          manifest.name
          kernel_package)
    |> Option.expect ~msg:"repo workspace should contain kernel package"
  in
  let package_root = Path.(root / Path.v "kernel") in
  copy_tree ~src:manifest.path ~dst:package_root;
  Riot_model.Workspace.make
    ?name:workspace.name
    ~root
    ~packages:[ { manifest with path = package_root; relative_path = Path.v "kernel" } ]
    ~dependencies:workspace.dependencies
    ~dev_dependencies:workspace.dev_dependencies
    ~build_dependencies:workspace.build_dependencies
    ~profile_overrides:workspace.profile_overrides
    ~source_ignore_patterns:workspace.source_ignore_patterns
    ~target_dir:Path.(root / Path.v "target")
    ()

let mutate_kernel_event_source = fun workspace revision ->
  let path =
    Path.(workspace.Riot_model.Workspace.root / Path.v "kernel" / Path.v "src/async/event.ml")
  in
  let content =
    Fs.read_to_string path
    |> Result.expect ~msg:"failed to read benchmark kernel async event source"
  in
  let marker =
    "\nlet __riot_build2_partial_rebuild_marker_"
    ^ Int.to_string revision
    ^ " = "
    ^ Int.to_string revision
    ^ "\n"
  in
  Fs.write (content ^ marker) path
  |> Result.expect ~msg:"failed to mutate benchmark kernel async event source"

let run_kernel_build_with_executor = fun ~workspace ~parallelism ->
  let config = Config.make ~workspace ~parallelism () in
  let executor =
    Riot_build2.create_executor ~config ()
    |> Result.expect ~msg:"failed to create riot-build2 executor"
  in
  let result =
    Riot_build2.execute executor (kernel_build_intent ())
    |> Result.expect ~msg:"failed to execute riot-build2 intent"
  in
  (executor, result)

let run_kernel_build = fun ~workspace ~parallelism ->
  let (_executor, result) = run_kernel_build_with_executor ~workspace ~parallelism in
  result

let make_kernel_build_cold_bench = fun ~parallelism ->
  let workspace = load_repo_workspace () in
  fun () ->
    let workspace = clone_workspace_with_target workspace ~target_dir:(kernel_cold_target_dir ()) in
    run_kernel_build ~workspace ~parallelism
    |> expect_kernel_package_result

let make_kernel_build_warm_cache_bench = fun ~parallelism ->
  let workspace_cell = ref None in
  let workspace_or_create = fun () ->
    match !workspace_cell with
    | Some workspace -> workspace
    | None ->
        let workspace =
          clone_workspace_with_target
            (load_repo_workspace ())
            ~target_dir:(kernel_warm_cache_target_dir ())
        in
        run_kernel_build ~workspace ~parallelism
        |> expect_kernel_package_result;
        workspace_cell := Some workspace;
        workspace
  in
  fun () ->
    let workspace = workspace_or_create () in
    run_kernel_build ~workspace ~parallelism
    |> expect_kernel_cached_package_result

type kernel_partial_event_state = {
  workspace: Riot_model.Workspace.t;
  mutable revision: int;
}

let expect_kernel_partial_event_action_results = fun executor ->
  let results =
    Build_services.action_results executor
    |> List.filter
      ~fn:(fun result ->
        Riot_model.Package_name.equal
          result.Action_execution.ref_.package
          kernel_package)
  in
  let count_status status =
    results
    |> List.filter
      ~fn:(fun result ->
        match (result.Action_execution.status, status) with
        | (Action_execution.Cached _, `Cached)
        | (Executed _, `Executed)
        | (Failed _, `Failed) -> true
        | (Cached _, _)
        | (Executed _, _)
        | (Failed _, _) -> false)
    |> List.length
  in
  let cached = count_status `Cached in
  let executed = count_status `Executed in
  let failed = count_status `Failed in
  if
    Int.equal (List.length results) 220
    && Int.equal cached 218
    && Int.equal executed 2
    && Int.equal failed 0
  then
    ()
  else
    panic
      ("kernel partial event benchmark expected actions total=220 cached=218 executed=2 failed=0, got total="
      ^ Int.to_string (List.length results)
      ^ " cached="
      ^ Int.to_string cached
      ^ " executed="
      ^ Int.to_string executed
      ^ " failed="
      ^ Int.to_string failed)

let make_kernel_partial_event_change_bench = fun ~parallelism ->
  let state = ref None in
  let state_or_create = fun () ->
    match !state with
    | Some state -> state
    | None ->
        let root =
          Path.(Path.v "_bench" / Path.v ("kernel-partial-event-" ^ UUID.to_string (UUID.v4 ())))
        in
        let workspace = isolated_kernel_workspace root in
        run_kernel_build ~workspace ~parallelism
        |> expect_kernel_package_result;
        let created = { workspace; revision = 0 } in
        state := Some created;
        created
  in
  fun () ->
    let state = state_or_create () in
    state.revision <- Int.succ state.revision;
    mutate_kernel_event_source state.workspace state.revision;
    let (executor, result) =
      run_kernel_build_with_executor ~workspace:state.workspace ~parallelism
    in
    expect_kernel_package_result result;
    expect_kernel_partial_event_action_results executor

let report_duration_json = fun duration ->
  Data.Json.obj [
    ("nanos", Data.Json.int (Int64.to_int (Time.Duration.to_nanos duration)));
    ("millis", Data.Json.float (Int64.to_float (Time.Duration.to_nanos duration) /. 1_000_000.0));
  ]

let kernel_action_timing_report = fun ~scenario ~parallelism ~wall_time executor ->
  let results =
    Build_services.action_results executor
    |> Action_timing_summary.for_package kernel_package
  in
  Data.Json.obj [
    ("type", Data.Json.string "RiotBuild2ActionTimingSummary");
    ("scenario", Data.Json.string scenario);
    ("package", Data.Json.string (Riot_model.Package_name.to_string kernel_package));
    ("parallelism", Data.Json.int parallelism);
    ("wall_time", report_duration_json wall_time);
    ("actions", Action_timing_summary.to_json (Action_timing_summary.of_results results));
  ]

let print_kernel_action_timing_report = fun ~scenario ~parallelism ~wall_time executor ->
  kernel_action_timing_report ~scenario ~parallelism ~wall_time executor
  |> Data.Json.to_string_pretty
  |> println

let run_kernel_cold_action_timing_summary = fun ~parallelism ->
  let workspace =
    clone_workspace_with_target (load_repo_workspace ()) ~target_dir:(kernel_cold_target_dir ())
  in
  let (executor_and_result, wall_time) =
    Timer.measure (fun () -> run_kernel_build_with_executor ~workspace ~parallelism)
  in
  let (executor, result) = executor_and_result in
  expect_kernel_package_result result;
  print_kernel_action_timing_report ~scenario:"kernel-cold" ~parallelism ~wall_time executor;
  Ok ()

let run_kernel_partial_event_action_timing_summary = fun ~parallelism ->
  let root =
    Path.(Path.v "_bench" / Path.v ("kernel-timing-partial-" ^ UUID.to_string (UUID.v4 ())))
  in
  let workspace = isolated_kernel_workspace root in
  run_kernel_build ~workspace ~parallelism
  |> expect_kernel_package_result;
  mutate_kernel_event_source workspace 1;
  let (executor_and_result, wall_time) =
    Timer.measure (fun () -> run_kernel_build_with_executor ~workspace ~parallelism)
  in
  let (executor, result) = executor_and_result in
  expect_kernel_package_result result;
  expect_kernel_partial_event_action_results executor;
  print_kernel_action_timing_report ~scenario:"kernel-partial-event" ~parallelism ~wall_time executor;
  Ok ()

let rec find_action_timing_summary = fun __tmp1 ->
  match __tmp1 with
  | "action-timing-summary" :: scenario :: _ -> Some scenario
  | _ :: rest -> find_action_timing_summary rest
  | [] -> None

let run_action_timing_summary = fun scenario ->
  let parallelism = Std.Thread.available_parallelism in
  match scenario with
  | "kernel-cold"
  | "cold" -> run_kernel_cold_action_timing_summary ~parallelism
  | "kernel-partial-event"
  | "partial-event" -> run_kernel_partial_event_action_timing_summary ~parallelism
  | other ->
      Error (Failure (
        "unknown action timing summary scenario "
        ^ other
        ^ " (expected kernel-cold or kernel-partial-event)"
      ))

let pure_config: Bench.bench_config = { iterations = 160; warmup = 20 }

let registry_config: Bench.bench_config = { iterations = 120; warmup = 16 }

let executor_bench_config: Bench.bench_config = { iterations = 40; warmup = 8 }

let runner_heavy_config: Bench.bench_config = { iterations = 20; warmup = 4 }

let planning_config: Bench.bench_config = { iterations = 80; warmup = 12 }

let workspace_load_config: Bench.bench_config = { iterations = 20; warmup = 4 }

let stage_config: Bench.bench_config = { iterations = 80; warmup = 12 }

let action_hash_config: Bench.bench_config = { iterations = 120; warmup = 16 }

let action_execute_config: Bench.bench_config = { iterations = 5; warmup = 1 }

let action_cached_config: Bench.bench_config = { iterations = 30; warmup = 4 }

let kernel_cold_build_config: Bench.bench_config = { iterations = 4; warmup = 1 }

let kernel_warm_cache_config: Bench.bench_config = { iterations = 20; warmup = 4 }

let kernel_partial_event_config: Bench.bench_config = { iterations = 6; warmup = 1 }

let benchmarks = fun () ->
  Bench.[
    with_config
      ~config:pure_config
      "riot-build2 intent expand build 100 packages x 2 targets"
      (make_intent_expand_build_bench ~package_count:100);
    with_config
      ~config:registry_config
      "riot-build2 registry intern 1000 unique goals"
      (make_registry_intern_unique_actions_bench ~count:1_000);
    with_config
      ~config:registry_config
      "riot-build2 registry find 1000 goal hits"
      (make_registry_find_goal_hits_bench ~count:1_000);
    with_config
      ~config:executor_bench_config
      ("riot-build2 executor drain 64 independent goals parallelism "
      ^ available_parallelism_label)
      (make_executor_independent_actions_bench
        ~count:64
        ~parallelism:available_parallelism);
    with_config
      ~config:executor_bench_config
      ("riot-build2 executor spawn 64 actions parallelism " ^ available_parallelism_label)
      (make_executor_spawn_actions_bench ~count:64 ~parallelism:available_parallelism);
    with_config
      ~config:executor_bench_config
      ("riot-build2 executor dependency fanout 64 parallelism "
      ^ available_parallelism_label)
      (make_executor_dependency_fanout_bench ~count:64 ~parallelism:available_parallelism);
    with_config
      ~config:runner_heavy_config
      ("riot-build2 executor planned dependency chain 32 parallelism "
      ^ available_parallelism_label)
      (make_executor_dependency_chain_bench ~count:32 ~parallelism:available_parallelism);
    with_config
      ~config:workspace_load_config
      "riot-build2 workspace load repo through workspace manager"
      (make_workspace_load_bench ());
    with_config
      ~config:planning_config
      "riot-build2 package catalog create repo manifests"
      (make_package_catalog_create_bench ());
    with_config
      ~config:planning_config
      "riot-build2 kernel runtime package realization"
      (make_kernel_runtime_realize_bench ());
    with_config
      ~config:planning_config
      "riot-build2 kernel runtime package realization cached within execution"
      (make_kernel_runtime_realize_cached_bench ());
    with_config
      ~config:planning_config
      "riot-build2 kernel intent expands to concrete package goal"
      (make_kernel_intent_to_goal_bench ());
    with_config
      ~config:stage_config
      "riot-build2 module provider registry cached lookup"
      (make_module_provider_registry_lookup_bench ());
    with_config
      ~config:stage_config
      "riot-build2 module plan cache hit small package"
      (make_module_plan_cache_hit_bench ());
    with_config
      ~config:stage_config
      "riot-build2 module dependencies from cached module plan small package"
      (make_module_dependencies_from_cached_module_plan_bench ());
    with_config
      ~config:action_hash_config
      "riot-build2 native compile-library action hash 64 objects"
      (make_native_compile_library_action_hash_bench ~source_count:64);
    with_config
      ~config:action_hash_config
      "riot-build2 native compile-sources action hash 64 generated sources"
      (make_native_compile_sources_action_hash_bench ~source_count:64);
    with_config
      ~config:action_hash_config
      "riot-build2 native compile-source action hash generated source"
      (make_native_compile_source_action_hash_bench ());
    with_config
      ~config:action_hash_config
      "riot-build2 native compile-library action json roundtrip 64 objects"
      (make_native_compile_library_action_json_roundtrip_bench ~source_count:64);
    with_config
      ~config:action_hash_config
      "riot-build2 native compile-sources action json roundtrip 64 generated sources"
      (make_native_compile_sources_action_json_roundtrip_bench ~source_count:64);
    with_config
      ~config:action_hash_config
      "riot-build2 native compile-source action json roundtrip generated source"
      (make_native_compile_source_action_json_roundtrip_bench ());
    with_config
      ~config:action_execute_config
      "riot-build2 native compile-sources cold action execution 8 generated sources"
      (make_native_compile_sources_cold_execution_bench ~source_count:8);
    with_config
      ~config:action_cached_config
      "riot-build2 native compile-sources cached action execution 8 generated sources"
      (make_native_compile_sources_cached_execution_bench ~source_count:8);
    with_config
      ~config:kernel_cold_build_config
      "riot-build2 kernel cold boot + cold cache graph execution available parallelism"
      (make_kernel_build_cold_bench ~parallelism:available_parallelism);
    with_config
      ~config:kernel_warm_cache_config
      "riot-build2 kernel cold boot + warm cache graph execution available parallelism"
      (make_kernel_build_warm_cache_bench ~parallelism:available_parallelism);
    with_config
      ~config:kernel_partial_event_config
      "riot-build2 kernel partial rebuild after async event source change"
      (make_kernel_partial_event_change_bench ~parallelism:available_parallelism);
  ]

let main ~args =
  match find_action_timing_summary args with
  | Some scenario -> run_action_timing_summary scenario
  | None -> Bench.Cli.main ~name:"riot-build2 benchmarks" ~benchmarks:(benchmarks ()) ~args

let () = Runtime.run ~main ~args:Env.args ()
