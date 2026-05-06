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
  Riot_model.Workspace.make ~root ~target_dir:(Path.v "target") ~packages:[] ()

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

let test_action_scheduler_returns_empty_results_for_empty_graph = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"action_scheduler_empty"
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
      if List.length result.Action_scheduler.completed_actions != 0 then
        Error "expected empty graph to produce no action results"
      else
        match result.Action_scheduler.first_failure with
        | None -> Ok ()
        | Some _ -> Error "expected empty graph to have no action failures") with
  | Ok result -> result
  | Error err -> Error ("tempdir creation failed: " ^ IO.error_message err)

let test_action_scheduler_reports_first_failure_and_keeps_other_results = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"action_scheduler_failure"
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
          ~deps:[ (Riot_planner.Action_node.id failing_node) ]
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
        Action_scheduler.find_result result failing_node,
        Action_scheduler.find_result result dependent_node,
        Action_scheduler.find_result result success_node,
        result.Action_scheduler.first_failure
      ) with
      | (
          Some { status = Action_scheduler.Failed _; _ },
          Some { status = Action_scheduler.Skipped; _ },
          Some { status = Action_scheduler.Executed _; _ },
          Some (Action_scheduler.ExecutionFailed _)
        ) ->
          if success_exists then
            Ok ()
          else
            Error "expected independent success output to be created"
      | _ -> Error "expected failed, skipped, and executed action scheduler results") with
  | Ok result -> result
  | Error err -> Error ("tempdir creation failed: " ^ IO.error_message err)

let test_action_scheduler_reports_incomplete_action_graph = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"action_scheduler_incomplete"
    (fun tmpdir ->
      let package = make_package ~root:tmpdir ~name:"pkg" in
      let graph = Riot_planner.Action_graph.create () in
      let _node =
        make_node_in
          graph
          ~package
          ~actions:[
            Riot_planner.Action.WriteFile {
              destination = Path.v "output.txt";
              content = "output";
            };
          ]
          ~outs:[ Path.v "output.txt" ]
          ()
      in
      let completed_results = HashMap.create () in
      let result = Action_scheduler.summarize_completed ~action_graph:graph ~completed_results in
      match result.Action_scheduler.first_failure with
      | Some (Action_scheduler.ExecutionFailed { message })
        when String.contains message "incomplete actions" ->
          Ok ()
      | Some _ -> Error "expected incomplete action graph to surface as execution failure"
      | None -> Error "expected incomplete action graph to fail") with
  | Ok result -> result
  | Error err -> Error ("tempdir creation failed: " ^ IO.error_message err)

let tests =
  Test.[
    case
      "action scheduler: empty graph returns no results"
      test_action_scheduler_returns_empty_results_for_empty_graph;
    case
      "action scheduler: failure is surfaced while ready work still completes"
      test_action_scheduler_reports_first_failure_and_keeps_other_results;
    case
      "action scheduler: incomplete graph is a failure"
      test_action_scheduler_reports_incomplete_action_graph;
  ]

let name = "riot-build:action-scheduler"

let main ~args = Test.Cli.main ~name ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
