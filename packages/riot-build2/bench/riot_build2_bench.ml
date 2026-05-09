open Std
open Std.Bench
open Riot_build2

let package = fun name ->
  Riot_model.Package_name.from_string name
  |> Result.expect ~msg:("invalid package name: " ^ name)

let target = fun value ->
  Riot_model.Target.from_string value
  |> Result.expect ~msg:("invalid target triple: " ^ value)

let node_id = fun value -> Work_node.Node_id.of_int value

let linux = target "x86_64-unknown-linux-gnu"

let kernel_package = package "kernel"

let unexpected_node = fun node ->
  Error (Error.ExecutorInvariantViolated {
    message = "unexpected node in riot-build2 benchmark: "
    ^ Work_node.Node_id.to_string (Work_node.id node);
  })

let sample_goal = fun ?(args = []) name ->
  Goal.RunBinary {
    package = Some (package "std");
    binary = Some name;
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

let rec list_nth = fun values index ->
  match (values, index) with
  | (value :: _, 0) -> Some value
  | (_ :: rest, _) -> list_nth rest (index - 1)
  | ([], _) -> None

let seed = fun () -> Work_node.user_intent ~id:(node_id 1) (User_intent.run ~target:linux ())

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
    package = Goal.Package kernel_package;
    profile = Riot_model.Profile.debug;
    target;
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

let make_intent_expand_build_bench = fun ~package_count ->
  let packages = package_names package_count in
  let intent = User_intent.build ~packages ~targets:[ linux; Riot_model.Target.current ] () in
  fun () ->
    let expanded = Intent_planner.expand intent in
    if Int.equal (List.length expanded) (package_count * 2) then
      ()
    else
      panic "intent expansion benchmark produced unexpected goal count"

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
      Executor.run ~parallelism ~seeds ~execute:(fun _context _node -> Ok (Executor.Complete [])) ()
    in
    expect_no_failures "independent goals" summary count

let make_executor_spawn_actions_bench = fun ~count ~parallelism ->
  let goal_list = actions count in
  fun () ->
    let summary =
      Executor.run
        ~parallelism
        ~seeds:[ seed () ]
        ~execute:(fun _context node ->
          match Work_node.kind node with
          | Work_node.UserIntent _ ->
              let spawned = List.map goal_list ~fn:(fun action -> Work_node.GoalKey action) in
              Ok (Executor.Complete spawned)
          | Work_node.Goal _ -> Ok (Executor.Complete [])
          | _ -> unexpected_node node)
        ()
    in
    expect_no_failures "spawn goals" summary (Int.succ count)

let make_executor_dependency_fanout_bench = fun ~count ~parallelism ->
  let goal_list = actions count in
  fun () ->
    let attempts = Sync.Atomic.make 0 in
    let summary =
      Executor.run
        ~parallelism
        ~seeds:[ seed () ]
        ~execute:(fun _context node ->
          match Work_node.kind node with
          | Work_node.UserIntent _ ->
              let attempt = Sync.Atomic.fetch_and_add attempts 1 in
              if Int.equal attempt 0 then
                let dependencies =
                  List.map goal_list ~fn:(fun action -> Work_node.GoalKey action)
                in
                Ok (Executor.RequeueWithDependencies dependencies)
              else
                Ok (Executor.Complete [])
          | Work_node.Goal _ -> Ok (Executor.Complete [])
          | _ -> unexpected_node node)
        ()
    in
    expect_no_failures "dependency fanout" summary (Int.succ count)

let make_executor_dependency_waves_bench = fun ~waves ->
  let goal_list = actions waves in
  fun () ->
    let attempts = Sync.Atomic.make 0 in
    let summary =
      Executor.run
        ~parallelism:1
        ~seeds:[ seed () ]
        ~execute:(fun _context node ->
          match Work_node.kind node with
          | Work_node.UserIntent _ ->
              let attempt = Sync.Atomic.fetch_and_add attempts 1 in
              if attempt < waves then
                let action =
                  list_nth goal_list attempt
                  |> Option.expect ~msg:"dependency wave action should exist"
                in
                Ok (Executor.RequeueWithDependencies [ Work_node.GoalKey action ])
              else
                Ok (Executor.Complete [])
          | Work_node.Goal _ -> Ok (Executor.Complete [])
          | _ -> unexpected_node node)
        ()
    in
    expect_no_failures "dependency waves" summary (Int.succ waves)

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

let make_kernel_goal_to_package_work_bench = fun () ->
  let workspace = load_repo_workspace () in
  let catalog = Package_catalog.create workspace in
  let goal = kernel_goal Riot_model.Target.current in
  fun () ->
    match Goal_planner.expand catalog goal with
    | Ok [ Package_work.BuildLibrary build ] when Riot_model.Package_name.equal
      build.package
      kernel_package -> ()
    | Ok _ -> panic "kernel goal expansion benchmark produced unexpected package work"
    | Error error -> panic ("kernel goal expansion benchmark failed: " ^ Error.message error)

let make_kernel_build_warm_bench = fun ~parallelism ->
  let target_dir = Path.(Path.v "_build" / Path.v "riot-build2-bench" / Path.v "kernel-warm") in
  let workspace = clone_workspace_with_target (load_repo_workspace ()) ~target_dir in
  fun () ->
    let request =
      Build_request.make
        ~workspace
        ~packages:[ kernel_package ]
        ~targets:[ Riot_model.Target.current ]
        ~profile:Riot_model.Profile.debug
        ~parallelism
        ()
    in
    Riot_build2.build request
    |> expect_kernel_package_result

let pure_config: Bench.bench_config = { iterations = 160; warmup = 20 }

let registry_config: Bench.bench_config = { iterations = 120; warmup = 16 }

let runner_config: Bench.bench_config = { iterations = 40; warmup = 8 }

let runner_heavy_config: Bench.bench_config = { iterations = 20; warmup = 4 }

let planning_config: Bench.bench_config = { iterations = 80; warmup = 12 }

let workspace_load_config: Bench.bench_config = { iterations = 20; warmup = 4 }

let kernel_warm_build_config: Bench.bench_config = { iterations = 4; warmup = 1 }

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
      ~config:runner_config
      "riot-build2 executor drain 64 independent goals parallelism 4"
      (make_executor_independent_actions_bench ~count:64 ~parallelism:4);
    with_config
      ~config:runner_config
      "riot-build2 executor spawn 64 actions parallelism 4"
      (make_executor_spawn_actions_bench ~count:64 ~parallelism:4);
    with_config
      ~config:runner_config
      "riot-build2 executor dependency fanout 64 parallelism 4"
      (make_executor_dependency_fanout_bench ~count:64 ~parallelism:4);
    with_config
      ~config:runner_heavy_config
      "riot-build2 executor dependency waves 32 parallelism 1"
      (make_executor_dependency_waves_bench ~waves:32);
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
      "riot-build2 kernel goal expands to package work"
      (make_kernel_goal_to_package_work_bench ());
    with_config
      ~config:kernel_warm_build_config
      "riot-build2 kernel warm build graph execution parallelism 4"
      (make_kernel_build_warm_bench ~parallelism:4);
  ]

let main ~args = Bench.Cli.main ~name:"riot-build2 benchmarks" ~benchmarks:(benchmarks ()) ~args

let () = Runtime.run ~main ~args:Env.args ()
