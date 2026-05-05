open Std
open Riot_build
open Std.Collections
open Riot_model

module Action_scheduler = Riot_build.Internal.Action_scheduler
module Sandbox = Riot_build.Internal.Sandbox
module Test = Std.Test

let package_name = fun value ->
  Package_name.from_string value
  |> Result.expect ~msg:("expected valid package name: " ^ value)

let test_toolchain = fun () ->
  Riot_toolchain.init ~config:Riot_model.Toolchain_config.default
  |> Result.expect ~msg:"failed to initialize toolchain"

let test_build_target = Riot_model.Target.current

let make_workspace = fun root ->
  Riot_model.Workspace.make ~root ~target_dir:"target" ~packages:[] ()

let make_package = fun ~root ~name ->
  let package_name = package_name name in
  let path = Path.(root / Path.v "packages" / Path.v name) in
  Riot_model.Package.make
    ~name:package_name
    ~path
    ~relative_path:(Path.v ("packages/" ^ name))
    ~library:{ path = Path.v "src/lib.ml" }
    ()

let make_node_in = fun graph ~package ?(deps = []) ~actions ~outs () ->
  let spec =
    Riot_planner.Action_node.make
      ~actions
      ~outs
      ~srcs:[]
      ~package
      ~toolchain:(test_toolchain ())
      ~dependency_hashes:(fun _ -> Crypto.hash_string "")
      ~deps
  in
  Riot_planner.Action_graph.add_node graph spec

let node_id = fun (node: Riot_planner.Action_node.t) -> (Riot_planner.Action_node.id node)

let find_result = fun result (node: Riot_planner.Action_node.t) ->
  Action_scheduler.find_result
    result
    node

let test_execute_empty_graph_returns_no_results = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"executor_empty_graph"
    (fun tmpdir ->
      let workspace = make_workspace tmpdir in
      let sandbox = Sandbox.create ~workspace () ~package_name:(package_name "pkg") in
      let result =
        Action_scheduler.run
          ~action_graph:(Riot_planner.Action_graph.create ())
          ~sandbox
          ~store:(Riot_store.Store.create ~workspace)
          ~session_id:(Riot_model.Session_id.make ())
          ~build_target:test_build_target
          (test_toolchain ())
          ~concurrency:2
      in
      let _ = Sandbox.cleanup sandbox in
      if List.length result.Action_scheduler.completed_actions = 0 then
        Ok ()
      else
        Error "expected empty graph to produce no execution results") with
  | Ok result -> result
  | Error err -> Error ("tempdir creation failed: " ^ IO.error_message err)

let test_execute_runs_independent_actions = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"executor_independent"
    (fun tmpdir ->
      let workspace = make_workspace tmpdir in
      let store = Riot_store.Store.create ~workspace in
      let package = make_package ~root:tmpdir ~name:"pkg" in
      let graph = Riot_planner.Action_graph.create () in
      let node_a =
        make_node_in
          graph
          ~package
          ~actions:[
            Riot_planner.Action.WriteFile { destination = Path.v "a.txt"; content = "a" };
          ]
          ~outs:[ Path.v "a.txt" ]
          ()
      in
      let node_b =
        make_node_in
          graph
          ~package
          ~actions:[
            Riot_planner.Action.WriteFile { destination = Path.v "b.txt"; content = "b" };
          ]
          ~outs:[ Path.v "b.txt" ]
          ()
      in
      let sandbox = Sandbox.create ~workspace () ~package_name:package.Riot_model.Package.name in
      let result =
        Action_scheduler.run
          ~action_graph:graph
          ~sandbox
          ~store
          ~session_id:(Riot_model.Session_id.make ())
          ~build_target:test_build_target
          (test_toolchain ())
          ~concurrency:2
      in
      let output_a =
        Fs.exists Path.(Sandbox.get_dir sandbox / Path.v "a.txt")
        |> Result.unwrap_or ~default:false
      in
      let output_b =
        Fs.exists Path.(Sandbox.get_dir sandbox / Path.v "b.txt")
        |> Result.unwrap_or ~default:false
      in
      let result_a = find_result result node_a in
      let result_b = find_result result node_b in
      let _ = Sandbox.cleanup sandbox in
      match (result_a, result_b) with
      | (
          Some { status = Action_scheduler.Executed _; _ },
          Some { status = Action_scheduler.Executed _; _ }
        ) ->
          if output_a && output_b then
            Ok ()
          else
            Error "expected independent action outputs to be created"
      | _ -> Error "expected both independent actions to execute") with
  | Ok result -> result
  | Error err -> Error ("tempdir creation failed: " ^ IO.error_message err)

let test_execute_skips_dependent_action_after_failure = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"executor_dependency_failure"
    (fun tmpdir ->
      let workspace = make_workspace tmpdir in
      let store = Riot_store.Store.create ~workspace in
      let package = make_package ~root:tmpdir ~name:"pkg" in
      let graph = Riot_planner.Action_graph.create () in
      let failing_node =
        make_node_in
          graph
          ~package
          ~actions:[
            Riot_planner.Action.CopyFile {
              source = Path.v "missing.txt";
              destination = Path.v "fail.txt";
            };
          ]
          ~outs:[ Path.v "fail.txt" ]
          ()
      in
      let dependent_node =
        make_node_in
          graph
          ~package
          ~deps:[ node_id failing_node ]
          ~actions:[
            Riot_planner.Action.WriteFile {
              destination = Path.v "dependent.txt";
              content = "dependent";
            };
          ]
          ~outs:[ Path.v "dependent.txt" ]
          ()
      in
      let success_node =
        make_node_in
          graph
          ~package
          ~actions:[
            Riot_planner.Action.WriteFile {
              destination = Path.v "success.txt";
              content = "success";
            };
          ]
          ~outs:[ Path.v "success.txt" ]
          ()
      in
      Riot_planner.Action_graph.add_dependency graph dependent_node ~depends_on:failing_node;
      let sandbox = Sandbox.create ~workspace () ~package_name:package.Riot_model.Package.name in
      let result =
        Action_scheduler.run
          ~action_graph:graph
          ~sandbox
          ~store
          ~session_id:(Riot_model.Session_id.make ())
          ~build_target:test_build_target
          (test_toolchain ())
          ~concurrency:2
      in
      let success_exists =
        Fs.exists Path.(Sandbox.get_dir sandbox / Path.v "success.txt")
        |> Result.unwrap_or ~default:false
      in
      let _ = Sandbox.cleanup sandbox in
      match (
        find_result result failing_node,
        find_result result dependent_node,
        find_result result success_node
      ) with
      | (
          Some { status = Action_scheduler.Failed _; _ },
          Some { status = Action_scheduler.Skipped; _ },
          Some { status = Action_scheduler.Executed _; _ }
        ) ->
          if success_exists then
            Ok ()
          else
            Error "expected independent success output to be created"
      | _ -> Error "expected failed, skipped, and executed statuses") with
  | Ok result -> result
  | Error err -> Error ("tempdir creation failed: " ^ IO.error_message err)

let tests =
  Test.[
    case "scheduler returns no results for empty graph" test_execute_empty_graph_returns_no_results;
    case "scheduler runs independent actions" test_execute_runs_independent_actions;
    case
      "scheduler skips dependent action after failure"
      test_execute_skips_dependent_action_after_failure;
  ]

let name = "riot-build:executor-behavior"

let main ~args = Test.Cli.main ~name ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
