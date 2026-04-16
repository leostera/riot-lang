open Std
open Riot_build
open Std.Bench
open Std.Collections
open Riot_model
module Action_executor = Riot_build.Internal.Action_executor
module Action_scheduler = Riot_build.Internal.Action_scheduler
module Action_queue = Riot_build.Internal.Action_queue
module Sandbox = Riot_build.Internal.Sandbox
module Action_graph = Riot_planner.Action_graph
module Action_node = Riot_planner.Action_node
module Package = Riot_model.Package
module Workspace = Riot_model.Workspace

let test_toolchain = Riot_toolchain.init ~config:Riot_model.Toolchain_config.default
|> Result.expect ~msg:"riot-build bench toolchain init should succeed"

let write_file = fun path contents ->
  let parent =
    match Path.parent path with
    | Some parent -> parent
    | None -> Path.v "."
  in
  let _ = Fs.create_dir_all parent |> Result.expect ~msg:"create bench parent should succeed" in
  Fs.write contents path |> Result.expect ~msg:"write bench file should succeed"

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
    target_dir_root =
      Path.(root / Path.v "target");
    packages = [];
    dependencies = [];
    dev_dependencies = [];
    build_dependencies = [];
    profile_overrides = [];
  }

let package_name = fun value ->
  Package_name.from_string value |> Result.expect ~msg:("expected valid package name: " ^ value)

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

let executed_result = fun node_id ->
  let now = Time.Instant.now () in
  Action_queue.{
    node_id;
    status = Executed;
    ocamlc_warnings = [];
    duration = Time.Duration.zero;
    started_at = now;
    completed_at = now;
  }

let failed_result = fun node_id ->
  let now = Time.Instant.now () in
  Action_queue.{
    node_id;
    status = Failed (ExecutionFailed { message = "boom" });
    ocamlc_warnings = [];
    duration = Time.Duration.zero;
    started_at = now;
    completed_at = now;
  }

let queue_all = fun queue nodes -> List.for_each nodes ~fn:(Action_queue.queue queue)

let drain_queue_success = fun queue ~total_nodes ->
  let rec loop () =
    match Action_queue.next queue with
    | None ->
        if Action_queue.is_complete queue ~total_nodes then
          ()
        else
          panic "queue bench expected all nodes to be completed"
    | Some node ->
        Action_queue.mark_completed queue (executed_result node.id);
        loop ()
  in
  loop ()

let make_independent_nodes = fun ~package ~count ->
  let graph = Action_graph.create () in
  List.init
    ~count
    ~fn:(fun index ->
      make_node_in
        graph
        ~package
        ~actions:[
          Riot_planner.Action.WriteFile {
            destination = Path.v ("out-" ^ Int.to_string index ^ ".txt");
            content = "x"
          };
        ]
        ~outs:[ Path.v ("out-" ^ Int.to_string index ^ ".txt") ]
        ())

let make_chain_nodes = fun ~package ~count ->
  let graph = Action_graph.create () in
  let rec loop index (previous: Action_node.t option) acc =
    if index = count then
      List.reverse acc
    else
      let deps =
        match previous with
        | Some node -> [ node.id ]
        | None -> []
      in
      let node = make_node_in
        graph
        ~package
        ~deps
        ~actions:[
          Riot_planner.Action.WriteFile {
            destination = Path.v ("chain-" ^ Int.to_string index ^ ".txt");
            content = "x"
          };
        ]
        ~outs:[ Path.v ("chain-" ^ Int.to_string index ^ ".txt") ]
        () in
      (
        match previous with
        | Some prev -> Action_graph.add_dependency graph node ~depends_on:prev
        | None -> ()
      );
      loop (index + 1) (Some node) (node :: acc)
  in
  loop 0 None []

let make_failure_fanout_nodes = fun ~package ~dependents ->
  let graph = Action_graph.create () in
  let root = make_node_in
    graph
    ~package
    ~actions:[ Riot_planner.Action.WriteFile { destination = Path.v "root.txt"; content = "root" }; ]
    ~outs:[ Path.v "root.txt" ]
    () in
  let children =
    List.init ~count:dependents
      ~fn:(fun index ->
        let node = make_node_in
          graph
          ~package
          ~deps:[ root.id ]
          ~actions:[
            Riot_planner.Action.WriteFile {
              destination = Path.v ("child-" ^ Int.to_string index ^ ".txt");
              content = "child"
            };
          ]
          ~outs:[ Path.v ("child-" ^ Int.to_string index ^ ".txt") ]
          () in
        Action_graph.add_dependency graph node ~depends_on:root;
        node)
  in
  (root, children)

let make_queue_independent_bench = fun root ~count ->
  let workspace = make_workspace Path.(root / Path.v ("queue-independent-" ^ Int.to_string count)) in
  let package = make_package ~root:workspace.root ~name:"pkg" in
  let nodes = make_independent_nodes ~package ~count in
  fun () ->
    let queue = Action_queue.create () in
    queue_all queue nodes;
    drain_queue_success queue ~total_nodes:count

let make_queue_chain_bench = fun root ~count ->
  let workspace = make_workspace Path.(root / Path.v ("queue-chain-" ^ Int.to_string count)) in
  let package = make_package ~root:workspace.root ~name:"pkg" in
  let nodes = make_chain_nodes ~package ~count in
  fun () ->
    let queue = Action_queue.create () in
    queue_all queue (List.reverse nodes);
    drain_queue_success queue ~total_nodes:count

let make_queue_failure_fanout_bench = fun root ~dependents ->
  let workspace = make_workspace Path.(root / Path.v ("queue-failure-" ^ Int.to_string dependents)) in
  let package = make_package ~root:workspace.root ~name:"pkg" in
  let root_node, children = make_failure_fanout_nodes ~package ~dependents in
  let total_nodes = dependents + 1 in
  fun () ->
    let queue = Action_queue.create () in
    queue_all queue (children @ [ root_node ]);
    (
      match Action_queue.next queue with
      | Some node when node.id = root_node.id -> Action_queue.mark_completed
        queue
        (failed_result root_node.id)
      | Some _ -> panic "queue failure bench expected root node first"
      | None -> panic "queue failure bench expected root node"
    );
    let rec finish () =
      match Action_queue.next queue with
      | None ->
          if Action_queue.is_complete queue ~total_nodes then
            ()
          else
            panic "queue failure bench expected all nodes to be accounted for"
      | Some node ->
          Action_queue.mark_completed queue (executed_result node.id);
          finish ()
    in
    finish ()

let make_execute_node_write_miss_bench = fun root ~size ->
  let workspace = make_workspace Path.(root / Path.v ("execute-node-write-" ^ Int.to_string size)) in
  let store = Riot_store.Store.create ~workspace in
  let package = make_package ~root:workspace.root ~name:"pkg" in
  let session_id = Riot_model.Session_id.make () in
  let counter = ref 0 in
  fun () ->
    let iteration = !counter in
    counter := iteration + 1;
    let sandbox = Path.(workspace.root / Path.v "sandboxes" / Path.v (Int.to_string iteration)) in
    let _ = Fs.create_dir_all sandbox |> Result.expect ~msg:"create write bench sandbox should succeed" in
    let output = Path.v "out.txt" in
    let graph = Action_graph.create () in
    let node = make_node_in
      graph
      ~package
      ~actions:[
        Riot_planner.Action.WriteFile {
          destination = output;
          content = Int.to_string iteration ^ ":" ^ payload ~size ~seed:iteration
        };
      ]
      ~outs:[ output ]
      () in
    let result = Action_executor.execute_node
      ~completed:(HashMap.create ())
      ~store
      ~session_id
      test_toolchain
      sandbox
      node in
    cleanup_dir sandbox;
    match result.status with
    | Action_executor.Executed -> ()
    | Action_executor.Cached _
    | Action_executor.Failed _
    | Action_executor.Skipped -> panic "execute_node write miss bench expected executed result"

let make_execute_node_cache_hit_bench = fun root ~size ->
  let workspace = make_workspace Path.(root / Path.v ("execute-node-cache-" ^ Int.to_string size)) in
  let store = Riot_store.Store.create ~workspace in
  let package = make_package ~root:workspace.root ~name:"pkg" in
  let session_id = Riot_model.Session_id.make () in
  let output = Path.v "out.txt" in
  let graph = Action_graph.create () in
  let node = make_node_in
    graph
    ~package
    ~actions:[
      Riot_planner.Action.WriteFile { destination = output; content = payload ~size ~seed:0 };
    ]
    ~outs:[ output ]
    () in
  let warm_sandbox = Path.(workspace.root / Path.v "warm-sandbox") in
  let _ = Fs.create_dir_all warm_sandbox |> Result.expect ~msg:"create warm sandbox should succeed" in
  let warm_result = Action_executor.execute_node
    ~completed:(HashMap.create ())
    ~store
    ~session_id
    test_toolchain
    warm_sandbox
    node in
  (
    match warm_result.status with
    | Action_executor.Executed -> ()
    | Action_executor.Cached _
    | Action_executor.Failed _
    | Action_executor.Skipped -> panic "execute_node cache fixture expected executed result"
  );
  cleanup_dir warm_sandbox;
  let counter = ref 0 in
  fun () ->
    let iteration = !counter in
    counter := iteration + 1;
    let sandbox = Path.(workspace.root / Path.v "cache-sandboxes" / Path.v (Int.to_string iteration)) in
    let _ = Fs.create_dir_all sandbox |> Result.expect ~msg:"create cache hit sandbox should succeed" in
    let result = Action_executor.execute_node
      ~completed:(HashMap.create ())
      ~store
      ~session_id
      test_toolchain
      sandbox
      node in
    cleanup_dir sandbox;
    match result.status with
    | Action_executor.Cached _ -> ()
    | Action_executor.Executed
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
            content = payload ~size:1_024 ~seed:(seed + index)
          };
        ]
        ~outs:[ Path.v ("out-" ^ Int.to_string index ^ ".txt") ]
        ())

let make_execute_graph_miss_bench = fun root ~count ~concurrency ->
  let workspace = make_workspace Path.(root / Path.v ("execute-graph-miss-" ^ Int.to_string count)) in
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
    let result = Action_scheduler.run
      ~action_graph:graph
      ~sandbox
      ~store
      ~session_id
      test_toolchain
      ~concurrency in
    Sandbox.cleanup sandbox;
    if List.length (Action_scheduler.results result) = List.length nodes then
      ()
    else
      panic "execute graph miss bench expected all nodes to complete"

let make_execute_graph_cache_hit_bench = fun root ~count ~concurrency ->
  let workspace = make_workspace Path.(root / Path.v ("execute-graph-cache-" ^ Int.to_string count)) in
  let store = Riot_store.Store.create ~workspace in
  let package = make_package ~root:workspace.root ~name:"pkg" in
  let session_id = Riot_model.Session_id.make () in
  let graph = Action_graph.create () in
  let _ = make_execute_graph_nodes graph ~package ~count ~seed:0 in
  let warm_sandbox = Sandbox.create ~workspace () ~package_name:package.Package.name in
  let warm_result = Action_scheduler.run
    ~action_graph:graph
    ~sandbox:warm_sandbox
    ~store
    ~session_id
    test_toolchain
    ~concurrency in
  Sandbox.cleanup warm_sandbox;
  let all_warm = List.length (Action_scheduler.results warm_result) = count in
  if not all_warm then
    panic "execute graph cache fixture expected all nodes to complete";
  fun () ->
    let sandbox = Sandbox.create ~workspace () ~package_name:package.Package.name in
    let result = Action_scheduler.run
      ~action_graph:graph
      ~sandbox
      ~store
      ~session_id
      test_toolchain
      ~concurrency in
    Sandbox.cleanup sandbox;
    if List.length (Action_scheduler.results result) = count then
      ()
    else
      panic "execute graph cache hit bench expected all nodes to complete"

let queue_config: Bench.bench_config = { iterations = 120; warmup = 12 }

let node_config: Bench.bench_config = { iterations = 60; warmup = 8 }

let graph_config: Bench.bench_config = { iterations = 24; warmup = 4 }

let benchmark_suite = fun root ->
  Bench.[
    with_config
      ~config:queue_config
      "riot-build action queue independent 256 nodes"
      (make_queue_independent_bench root ~count:256);
    with_config
      ~config:queue_config
      "riot-build action queue dependency chain 256 nodes"
      (make_queue_chain_bench root ~count:256);
    with_config
      ~config:queue_config
      "riot-build action queue failure fanout 255 dependents"
      (make_queue_failure_fanout_bench root ~dependents:255);
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

let () =
  Actors.run
    ~main:(fun ~args ->
      match Fs.with_tempdir
        ~prefix:"riot_executor_bench"
        (fun root ->
          Bench.Cli.main ~name:"riot-build benchmarks" ~benchmarks:(benchmark_suite root) ~args) with
      | Ok result -> result
      | Error err -> panic ("failed to prepare riot-build bench fixture: " ^ IO.error_message err))
    ~args:Env.args
    ()
