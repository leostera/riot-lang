open Std
open Riot_build
open Std.Bench
open Std.Collections
open Riot_model

module Action_executor = Riot_build.Internal.Action_executor
module Action_scheduler = Riot_build.Internal.Action_scheduler
module Sandbox = Riot_build.Internal.Sandbox
module Action_graph = Riot_planner.Action_graph
module Action_node = Riot_planner.Action_node
module Package = Riot_model.Package
module Workspace = Riot_model.Workspace

let test_toolchain =
  Riot_toolchain.init ~config:Riot_model.Toolchain_config.default
  |> Result.expect ~msg:"riot-build bench toolchain init should succeed"

let test_build_target = Riot_model.Target.current

let cleanup_dir = fun path ->
  match Fs.remove_dir_all path with
  | Ok () -> ()
  | Error _ -> ()

let payload = fun ~size ~seed ->
  String.init
    ~len:size
    ~fn:(fun index -> Char.from_int_unchecked (Char.to_int 'a' + ((index + seed) mod 26)))

let make_workspace = fun root ->
  Workspace.{
    name = None;
    root;
    target_dir_root = Path.(root / Path.v "target");
    packages = [];
    dependencies = [];
    dev_dependencies = [];
    build_dependencies = [];
    profile_overrides = [];
  }

let package_name = fun value ->
  Package_name.from_string value
  |> Result.expect ~msg:("expected valid package name: " ^ value)

let make_package = fun ~root ~name ->
  let path = Path.(root / Path.v "packages" / Path.v name) in
  Package.make
    ~name:(package_name name)
    ~path
    ~relative_path:(Path.v ("packages/" ^ name))
    ~library:{ path = Path.v "src/lib.ml" }
    ()

let make_node_in = fun graph ~package ?(deps = []) ~actions ~outs () ->
  let spec =
    Action_node.make
      ~actions
      ~outs
      ~srcs:[]
      ~package
      ~toolchain:test_toolchain
      ~dependency_hashes:(fun _ -> Crypto.hash_string "")
      ~deps
  in
  Action_graph.add_node graph spec

let make_execute_node_write_miss_bench = fun root ~size ->
  let workspace =
    make_workspace Path.(root / Path.v ("execute-node-write-" ^ Int.to_string size))
  in
  let store = Riot_store.Store.create ~workspace in
  let package = make_package ~root:workspace.root ~name:"pkg" in
  let session_id = Riot_model.Session_id.make () in
  let counter = ref 0 in
  fun () ->
    let iteration = !counter in
    counter := iteration + 1;
    let sandbox = Path.(workspace.root / Path.v "sandboxes" / Path.v (Int.to_string iteration)) in
    let _ =
      Fs.create_dir_all sandbox
      |> Result.expect ~msg:"create write bench sandbox should succeed"
    in
    let output = Path.v "out.txt" in
    let graph = Action_graph.create () in
    let node =
      make_node_in
        graph
        ~package
        ~actions:[
          Riot_planner.Action.WriteFile {
            destination = output;
            content = Int.to_string iteration ^ ":" ^ payload ~size ~seed:iteration;
          };
        ]
        ~outs:[ output ]
        ()
    in
    let result =
      Action_executor.execute_node
        ~completed:(HashMap.create ())
        ~store
        ~session_id
        ~build_target:test_build_target
        test_toolchain
        sandbox
        node
    in
    cleanup_dir sandbox;
    match result.status with
    | Action_executor.Executed _ -> ()
    | Action_executor.Cached _
    | Action_executor.Failed _
    | Action_executor.Skipped -> panic "execute_node write miss bench expected executed result"

let make_execute_node_cache_hit_bench = fun root ~size ->
  let workspace =
    make_workspace Path.(root / Path.v ("execute-node-cache-" ^ Int.to_string size))
  in
  let store = Riot_store.Store.create ~workspace in
  let package = make_package ~root:workspace.root ~name:"pkg" in
  let session_id = Riot_model.Session_id.make () in
  let output = Path.v "out.txt" in
  let graph = Action_graph.create () in
  let node =
    make_node_in
      graph
      ~package
      ~actions:[
        Riot_planner.Action.WriteFile { destination = output; content = payload ~size ~seed:0 };
      ]
      ~outs:[ output ]
      ()
  in
  let warm_sandbox = Path.(workspace.root / Path.v "warm-sandbox") in
  let _ =
    Fs.create_dir_all warm_sandbox
    |> Result.expect ~msg:"create warm sandbox should succeed"
  in
  let warm_result =
    Action_executor.execute_node
      ~completed:(HashMap.create ())
      ~store
      ~session_id
      ~build_target:test_build_target
      test_toolchain
      warm_sandbox
      node
  in
  (
    match warm_result.status with
    | Action_executor.Executed _ -> ()
    | Action_executor.Cached _
    | Action_executor.Failed _
    | Action_executor.Skipped -> panic "execute_node cache fixture expected executed result"
  );
  cleanup_dir warm_sandbox;
  let counter = ref 0 in
  fun () ->
    let iteration = !counter in
    counter := iteration + 1;
    let sandbox =
      Path.(workspace.root / Path.v "cache-sandboxes" / Path.v (Int.to_string iteration))
    in
    let _ =
      Fs.create_dir_all sandbox
      |> Result.expect ~msg:"create cache hit sandbox should succeed"
    in
    let result =
      Action_executor.execute_node
        ~completed:(HashMap.create ())
        ~store
        ~session_id
        ~build_target:test_build_target
        test_toolchain
        sandbox
        node
    in
    cleanup_dir sandbox;
    match result.status with
    | Action_executor.Cached _ -> ()
    | Action_executor.Executed _
    | Action_executor.Failed _
    | Action_executor.Skipped -> panic "execute_node cache hit bench expected cached result"

let make_execute_graph_nodes = fun graph ~package ~count ~seed ->
  List.init
    ~count
    ~fn:(fun index ->
      make_node_in
        graph
        ~package
        ~actions:[
          Riot_planner.Action.WriteFile {
            destination = Path.v ("out-" ^ Int.to_string index ^ ".txt");
            content = payload ~size:1_024 ~seed:(seed + index);
          };
        ]
        ~outs:[ Path.v ("out-" ^ Int.to_string index ^ ".txt") ]
        ())

let make_execute_graph_miss_bench = fun root ~count ~concurrency ->
  let workspace =
    make_workspace Path.(root / Path.v ("execute-graph-miss-" ^ Int.to_string count))
  in
  let store = Riot_store.Store.create ~workspace in
  let package = make_package ~root:workspace.root ~name:"pkg" in
  let session_id = Riot_model.Session_id.make () in
  let counter = ref 0 in
  fun () ->
    let iteration = !counter in
    counter := iteration + 1;
    let graph = Action_graph.create () in
    let nodes = make_execute_graph_nodes graph ~package ~count ~seed:(iteration * count) in
    let sandbox = Sandbox.create ~workspace () ~package_name:package.Package.name in
    let result =
      Action_scheduler.run
        ~action_graph:graph
        ~sandbox
        ~store
        ~session_id
        ~build_target:test_build_target
        test_toolchain
        ~concurrency
    in
    Sandbox.cleanup sandbox;
    if List.length result.Action_scheduler.completed_actions = List.length nodes then
      ()
    else
      panic "execute graph miss bench expected all nodes to complete"

let make_execute_graph_cache_hit_bench = fun root ~count ~concurrency ->
  let workspace =
    make_workspace Path.(root / Path.v ("execute-graph-cache-" ^ Int.to_string count))
  in
  let store = Riot_store.Store.create ~workspace in
  let package = make_package ~root:workspace.root ~name:"pkg" in
  let session_id = Riot_model.Session_id.make () in
  let graph = Action_graph.create () in
  let _ = make_execute_graph_nodes graph ~package ~count ~seed:0 in
  let warm_sandbox = Sandbox.create ~workspace () ~package_name:package.Package.name in
  let warm_result =
    Action_scheduler.run
      ~action_graph:graph
      ~sandbox:warm_sandbox
      ~store
      ~session_id
      ~build_target:test_build_target
      test_toolchain
      ~concurrency
  in
  Sandbox.cleanup warm_sandbox;
  let all_warm = List.length warm_result.Action_scheduler.completed_actions = count in
  if not all_warm then
    panic "execute graph cache fixture expected all nodes to complete";
  fun () ->
    let sandbox = Sandbox.create ~workspace () ~package_name:package.Package.name in
    let result =
      Action_scheduler.run
        ~action_graph:graph
        ~sandbox
        ~store
        ~session_id
        ~build_target:test_build_target
        test_toolchain
        ~concurrency
    in
    Sandbox.cleanup sandbox;
    if List.length result.Action_scheduler.completed_actions = count then
      ()
    else
      panic "execute graph cache hit bench expected all nodes to complete"

let node_config: Bench.bench_config = { iterations = 60; warmup = 8 }

let graph_config: Bench.bench_config = { iterations = 24; warmup = 4 }

let benchmark_suite = fun root ->
  Bench.[
    with_config
      ~config:node_config
      "riot-build execute_node write miss 4kb"
      (make_execute_node_write_miss_bench root ~size:4_096);
    with_config
      ~config:node_config
      "riot-build execute_node cache hit 4kb"
      (make_execute_node_cache_hit_bench root ~size:4_096);
    with_config
      ~config:graph_config
      "riot-build execute graph miss 16 writes concurrency 4"
      (make_execute_graph_miss_bench root ~count:16 ~concurrency:4);
    with_config
      ~config:graph_config
      "riot-build execute graph cache hit 16 writes concurrency 4"
      (make_execute_graph_cache_hit_bench root ~count:16 ~concurrency:4);
  ]

let main ~args =
  match Fs.with_tempdir
    ~prefix:"riot_executor_bench"
    (fun root ->
      Bench.Cli.main
        ~name:"riot-build benchmarks"
        ~benchmarks:(benchmark_suite root)
        ~args) with
  | Ok result -> result
  | Error err -> panic ("failed to prepare riot-build bench fixture: " ^ IO.error_message err)

let () = Runtime.run ~main ~args:Env.args ()
