open Std
open Riot_build
open Riot_model
module Test = Std.Test
module G = Graph.SimpleGraph

let package_name = fun value ->
  Package_name.from_string value |> Result.expect ~msg:("expected valid package name: " ^ value)

let test_toolchain = fun () ->
  Riot_toolchain.init ~config:Riot_model.Toolchain_config.default |> Result.expect ~msg:"failed to initialize toolchain"

let make_test_package = fun name ->
  Riot_model.Package.make
    ~name:(package_name name)
    ~path:(Path.v ("packages/" ^ name))
    ~relative_path:(Path.v ("packages/" ^ name))
    ~library:{ path = Path.v "src/lib.ml" }
    ()

let make_action_node = fun ?(deps = []) ?(outs = []) ?(actions = []) package_name ->
  let graph = Riot_planner.Action_graph.create () in
  let package = make_test_package package_name in
  let spec =
    Riot_planner.Action_node.make
      ~actions
      ~outs
      ~srcs:[]
      ~package
      ~toolchain:(test_toolchain ())
      ~dependency_hashes:(fun _ -> Crypto.hash_string "dep")
      ~deps
  in
  Riot_planner.Action_graph.add_node graph spec

let make_action_node_in = fun graph ?(deps = []) ?(outs = []) ?(actions = []) package_name ->
  let package = make_test_package package_name in
  let spec =
    Riot_planner.Action_node.make
      ~actions
      ~outs
      ~srcs:[]
      ~package
      ~toolchain:(test_toolchain ())
      ~dependency_hashes:(fun _ -> Crypto.hash_string "dep")
      ~deps
  in
  Riot_planner.Action_graph.add_node graph spec

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

let queue_respects_dependency_order = fun _ctx ->
  let queue = Action_queue.create () in
  let graph = Riot_planner.Action_graph.create () in
  let dep_node = make_action_node_in
    graph
    "kernel"
    ~actions:[
      Riot_planner.Action.WriteFile { destination = Path.v "kernel.txt"; content = "kernel" };
    ] in
  let app_node = make_action_node_in
    graph
    "app"
    ~deps:[ dep_node.id ]
    ~actions:[ Riot_planner.Action.WriteFile { destination = Path.v "app.txt"; content = "app" }; ] in
  Riot_planner.Action_graph.add_dependency graph app_node ~depends_on:dep_node;
  Action_queue.queue queue app_node;
  Action_queue.queue queue dep_node;
  match Action_queue.next queue with
  | None -> Error "expected dependency node first"
  | Some first ->
      Test.assert_true (G.Node_id.eq first.id dep_node.id);
      Action_queue.mark_completed queue (executed_result dep_node.id);
      (
        match Action_queue.next queue with
        | None -> Error "expected dependent node after dependency completion"
        | Some second ->
            Test.assert_true (G.Node_id.eq second.id app_node.id);
            Ok ()
      )

let queue_marks_dependents_skipped_after_failure = fun _ctx ->
  let queue = Action_queue.create () in
  let graph = Riot_planner.Action_graph.create () in
  let dep_node = make_action_node_in graph "std" in
  let dependent_node = make_action_node_in graph "riot-model" ~deps:[ dep_node.id ] in
  Riot_planner.Action_graph.add_dependency graph dependent_node ~depends_on:dep_node;
  Action_queue.queue queue dependent_node;
  Action_queue.queue queue dep_node;
  let _ = Action_queue.next queue in
  Action_queue.mark_completed queue (failed_result dep_node.id);
  let _ = Action_queue.next queue in
  match Action_queue.get_result queue dependent_node.id with
  | Some { status=Skipped; _ } -> Ok ()
  | Some _ -> Error "expected dependent action to be skipped"
  | None -> Error "missing dependent result"

let requeue_with_deps_moves_blocked_node_and_queues_missing_dependency = fun _ctx ->
  let queue = Action_queue.create () in
  let missing_dep = make_action_node "kernel" in
  let blocked = make_action_node "std" ~deps:[ missing_dep.id ] in
  blocked.deps <- [ missing_dep.id ];
  Action_queue.queue queue blocked;
  let _ = Action_queue.next queue in
  Action_queue.requeue_with_deps
    queue
    blocked
    ~missing_deps:[ missing_dep.id ]
    ~all_nodes:[ blocked; missing_dep ];
  match Action_queue.next queue with
  | Some ready ->
      Test.assert_true (G.Node_id.eq ready.id missing_dep.id);
      Ok ()
  | None -> Error "expected missing dependency node to be queued"

let is_complete_checks_all_nodes_accounted_for = fun _ctx ->
  let queue = Action_queue.create () in
  let node_a = make_action_node "a" in
  let node_b = make_action_node "b" in
  Action_queue.queue queue node_a;
  Action_queue.queue queue node_b;
  let _ = Action_queue.next queue in
  Action_queue.mark_completed queue (executed_result node_a.id);
  let _ = Action_queue.next queue in
  Action_queue.mark_completed queue (executed_result node_b.id);
  if Action_queue.is_complete queue ~total_nodes:2 then
    Ok ()
  else
    Error "queue should be complete after both nodes finish"

let tests =
  Test.[
    case "queue respects dependency order" queue_respects_dependency_order;
    case "queue marks dependents skipped after failure" queue_marks_dependents_skipped_after_failure;
    case "requeue_with_deps moves blocked node and queues missing dependency" requeue_with_deps_moves_blocked_node_and_queues_missing_dependency;
    case "is_complete checks all nodes accounted for" is_complete_checks_all_nodes_accounted_for;
  ]

let name = "riot-build:action-queue-workspace-graph"

let () = Actors.run ~main:(Test.Cli.main ~name ~tests) ~args:Env.args ()
