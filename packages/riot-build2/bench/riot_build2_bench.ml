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
  Riot_model.Workspace.make
    ~root
    ~target_dir:Path.(root / Path.v "target")
    ~packages:[ package ]
    ()

let source_build = fun () ->
  Goal.{
    package = package "sourcepkg";
    scope = Goal.Runtime;
    profile = Riot_model.Profile.debug;
    target = Riot_model.Target.current;
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
              let spawned = List.map goal_list ~fn:(fun action -> Work_node.GoalKey action) in
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
          Ok (List.map goal_list ~fn:(fun action -> Work_node.GoalKey action))
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

let make_executor_dependency_chain_bench = fun ~count ->
  let goal_list = actions count in
  let rec next_dependency = fun action actions ->
    match actions with
    | current :: next :: _ when current = action -> Some next
    | _ :: rest -> next_dependency action rest
    | [] -> None
  in
  let intent_dependencies =
    match goal_list with
    | first :: _ -> Ok [ Work_node.GoalKey first ]
    | [] -> Ok []
  in
  let action_dependencies = fun action ->
    match next_dependency action goal_list with
    | Some dependency -> Ok [ Work_node.GoalKey dependency ]
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
        ~config:(executor_config 1)
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

let execute_module_plan_to_cache = fun workspace ->
  let config = Config.make ~workspace ~parallelism:1 () in
  let services = Build_services.create ~config () in
  let registry = Work_registry.create () in
  let node = Work_node.module_plan ~id:(node_id 1) (source_build ()) in
  let source_keys =
    match Build_services.execute_node services registry node with
    | Ok (Work_result.RequeueWithDependencies keys) ->
        List.filter
          keys
          ~fn:(fun __tmp1 ->
            match __tmp1 with
            | Work_node.SourceAnalysisKey _ -> true
            | _ -> false)
    | Ok _ -> panic "module plan cache setup expected source analysis dependencies"
    | Error error -> panic ("module plan cache setup failed: " ^ Error.message error)
  in
  List.for_each
    source_keys
    ~fn:(fun key ->
      match Work_registry.find registry key with
      | None -> panic "module plan cache setup missed source analysis node"
      | Some source_node ->
          Build_services.execute_node services registry source_node
          |> Result.expect ~msg:"source analysis cache setup failed"
          |> ignore);
  match Build_services.execute_node services registry node with
  | Ok (Work_result.Complete []) -> ()
  | Ok _ -> panic "module plan cache setup did not complete module plan"
  | Error error -> panic ("module plan cache setup failed: " ^ Error.message error)

let make_module_plan_cache_hit_bench = fun () ->
  let workspace =
    source_package_workspace
      Path.(Path.v "_bench" / Path.v ("module-plan-cache-" ^ UUID.to_string (UUID.v4 ())))
  in
  execute_module_plan_to_cache workspace;
  fun () ->
    let config = Config.make ~workspace ~parallelism:1 () in
    let services = Build_services.create ~config () in
    let registry = Work_registry.create () in
    let node = Work_node.module_plan ~id:(node_id 1) (source_build ()) in
    match Build_services.execute_node services registry node with
    | Ok (Work_result.Complete []) -> ()
    | Ok _ -> panic "module plan cache hit benchmark requested dependencies"
    | Error error -> panic ("module plan cache hit benchmark failed: " ^ Error.message error)

let make_action_plan_from_cached_module_plan_bench = fun () ->
  let workspace =
    source_package_workspace
      Path.(Path.v "_bench" / Path.v ("action-plan-cache-" ^ UUID.to_string (UUID.v4 ())))
  in
  execute_module_plan_to_cache workspace;
  fun () ->
    let config = Config.make ~workspace ~parallelism:1 () in
    let services = Build_services.create ~config () in
    let registry = Work_registry.create () in
    let build = source_build () in
    let module_node = Work_node.module_plan ~id:(node_id 1) build in
    let action_node = Work_node.action_plan ~id:(node_id 2) build in
    (
      match Build_services.execute_node services registry module_node with
      | Ok (Work_result.Complete []) -> ()
      | Ok _ -> panic "action plan benchmark module plan cache hit requested dependencies"
      | Error error -> panic ("action plan benchmark module plan failed: " ^ Error.message error)
    );
    match Build_services.execute_node services registry action_node with
    | Ok (Work_result.Complete []) -> ()
    | Ok _ -> panic "action plan benchmark requested dependencies"
    | Error error -> panic ("action plan benchmark failed: " ^ Error.message error)

let kernel_cold_target_dir = fun () ->
  Path.(Path.v "_bench" / Path.v ("kernel-" ^ UUID.to_string (UUID.v4 ())))

let kernel_warm_cache_target_dir = fun () ->
  Path.(Path.v "_build"
  / Path.v "riot-build2-bench"
  / Path.v ("kernel-warm-cache-" ^ UUID.to_string (UUID.v4 ())))

let run_kernel_build = fun ~workspace ~parallelism ->
  let config = Config.make ~workspace ~parallelism () in
  let executor =
    Riot_build2.create_executor ~config ()
    |> Result.expect ~msg:"failed to create riot-build2 executor"
  in
  Riot_build2.execute executor (kernel_build_intent ())
  |> Result.expect ~msg:"failed to execute riot-build2 intent"

let make_kernel_build_cold_bench = fun ~parallelism ->
  let workspace = load_repo_workspace () in
  fun () ->
    let workspace = clone_workspace_with_target workspace ~target_dir:(kernel_cold_target_dir ()) in
    run_kernel_build ~workspace ~parallelism
    |> expect_kernel_package_result

let make_kernel_build_warm_cache_bench = fun ~parallelism ->
  let workspace =
    clone_workspace_with_target
      (load_repo_workspace ())
      ~target_dir:(kernel_warm_cache_target_dir ())
  in
  run_kernel_build ~workspace ~parallelism
  |> expect_kernel_package_result;
  fun () ->
    run_kernel_build ~workspace ~parallelism
    |> expect_kernel_cached_package_result

let pure_config: Bench.bench_config = { iterations = 160; warmup = 20 }

let registry_config: Bench.bench_config = { iterations = 120; warmup = 16 }

let executor_bench_config: Bench.bench_config = { iterations = 40; warmup = 8 }

let runner_heavy_config: Bench.bench_config = { iterations = 20; warmup = 4 }

let planning_config: Bench.bench_config = { iterations = 80; warmup = 12 }

let workspace_load_config: Bench.bench_config = { iterations = 20; warmup = 4 }

let stage_config: Bench.bench_config = { iterations = 80; warmup = 12 }

let kernel_cold_build_config: Bench.bench_config = { iterations = 4; warmup = 1 }

let kernel_warm_cache_config: Bench.bench_config = { iterations = 20; warmup = 4 }

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
      "riot-build2 executor drain 64 independent goals parallelism 4"
      (make_executor_independent_actions_bench ~count:64 ~parallelism:4);
    with_config
      ~config:executor_bench_config
      "riot-build2 executor spawn 64 actions parallelism 4"
      (make_executor_spawn_actions_bench ~count:64 ~parallelism:4);
    with_config
      ~config:executor_bench_config
      "riot-build2 executor dependency fanout 64 parallelism 4"
      (make_executor_dependency_fanout_bench ~count:64 ~parallelism:4);
    with_config
      ~config:runner_heavy_config
      "riot-build2 executor planned dependency chain 32 parallelism 1"
      (make_executor_dependency_chain_bench ~count:32);
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
      "riot-build2 action plan from cached module plan small package"
      (make_action_plan_from_cached_module_plan_bench ());
    with_config
      ~config:kernel_cold_build_config
      "riot-build2 kernel cold boot + cold cache graph execution available parallelism"
      (make_kernel_build_cold_bench ~parallelism:Std.Thread.available_parallelism);
    with_config
      ~config:kernel_warm_cache_config
      "riot-build2 kernel cold boot + warm cache graph execution parallelism 4"
      (make_kernel_build_warm_cache_bench ~parallelism:Std.Thread.available_parallelism);
  ]

let main ~args = Bench.Cli.main ~name:"riot-build2 benchmarks" ~benchmarks:(benchmarks ()) ~args

let () = Runtime.run ~main ~args:Env.args ()
