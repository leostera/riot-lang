open Std
open Riot_build
open Std.Collections
open Riot_model

module Action_executor = Riot_build.Internal.Action_executor
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

let make_node = fun ~package ?(dep_nodes = []) ~actions ~outs () ->
  let graph = Riot_planner.Action_graph.create () in
  let deps = List.map dep_nodes ~fn:(fun (node: Riot_planner.Action_node.t) -> (Riot_planner.Action_node.id node)) in
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
  let node = Riot_planner.Action_graph.add_node graph spec in
  let _ =
    List.for_each
      dep_nodes
      ~fn:(fun dep_node ->
        Riot_planner.Action_graph.add_dependency graph node ~depends_on:dep_node)
  in
  node

let node_id = fun (node: Riot_planner.Action_node.t) -> (Riot_planner.Action_node.id node)

let failed_result = fun node_id ->
  let now = Time.Instant.now () in
  Action_executor.{
    node_id;
    status = Failed (ExecutionFailed { message = "boom" });
    ocamlc_warnings = [];
    duration = Time.Duration.zero;
    started_at = now;
    completed_at = now;
  }

let test_execute_node_writes_file = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"action_executor_write"
    (fun tmpdir ->
      let sandbox = Path.(tmpdir / Path.v "sandbox") in
      let _ =
        Fs.create_dir_all sandbox
        |> Result.expect ~msg:"create sandbox failed"
      in
      let workspace = make_workspace tmpdir in
      let store = Riot_store.Store.create ~workspace in
      let package = make_package ~root:tmpdir ~name:"pkg" in
      let output = Path.v "out.txt" in
      let node =
        make_node
          ~package
          ~actions:[ Riot_planner.Action.WriteFile { destination = output; content = "hello" } ]
          ~outs:[ output ]
          ()
      in
      let completed = HashMap.create () in
      let result =
        Action_executor.execute_node
          ~completed
          ~store
          ~session_id:(Riot_model.Session_id.make ())
          ~build_target:test_build_target
          (test_toolchain ())
          sandbox
          node
      in
      match result.status with
      | Action_executor.Executed _ -> (
          let output_path = Path.(sandbox / output) in
          match Fs.read_to_string output_path with
          | Ok content when String.equal content "hello" -> Ok ()
          | Ok content -> Error ("unexpected output content: " ^ content)
          | Error err -> Error ("failed to read output: " ^ IO.error_message err)
        )
      | Action_executor.Cached _
      | Action_executor.Failed _
      | Action_executor.Skipped -> Error "expected write action to execute") with
  | Ok result -> result
  | Error err -> Error ("tempdir creation failed: " ^ IO.error_message err)

let test_execute_node_copies_file = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"action_executor_copy"
    (fun tmpdir ->
      let sandbox = Path.(tmpdir / Path.v "sandbox") in
      let _ =
        Fs.create_dir_all sandbox
        |> Result.expect ~msg:"create sandbox failed"
      in
      let source = Path.(sandbox / Path.v "source.txt") in
      let _ =
        Fs.write "copy me" source
        |> Result.expect ~msg:"write source failed"
      in
      let workspace = make_workspace tmpdir in
      let store = Riot_store.Store.create ~workspace in
      let package = make_package ~root:tmpdir ~name:"pkg" in
      let destination = Path.v "copied.txt" in
      let node =
        make_node
          ~package
          ~actions:[ Riot_planner.Action.CopyFile { source = Path.v "source.txt"; destination } ]
          ~outs:[ destination ]
          ()
      in
      let completed = HashMap.create () in
      let result =
        Action_executor.execute_node
          ~completed
          ~store
          ~session_id:(Riot_model.Session_id.make ())
          ~build_target:test_build_target
          (test_toolchain ())
          sandbox
          node
      in
      match result.status with
      | Action_executor.Executed _ -> (
          let destination_path = Path.(sandbox / destination) in
          match Fs.read_to_string destination_path with
          | Ok content when String.equal content "copy me" -> Ok ()
          | Ok content -> Error ("unexpected copied content: " ^ content)
          | Error err -> Error ("failed to read copied file: " ^ IO.error_message err)
        )
      | Action_executor.Cached _
      | Action_executor.Failed _
      | Action_executor.Skipped -> Error "expected copy action to execute") with
  | Ok result -> result
  | Error err -> Error ("tempdir creation failed: " ^ IO.error_message err)

let test_execute_node_fails_when_declared_output_is_missing = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"action_executor_missing_output"
    (fun tmpdir ->
      let sandbox = Path.(tmpdir / Path.v "sandbox") in
      let _ =
        Fs.create_dir_all sandbox
        |> Result.expect ~msg:"create sandbox failed"
      in
      let workspace = make_workspace tmpdir in
      let store = Riot_store.Store.create ~workspace in
      let package = make_package ~root:tmpdir ~name:"pkg" in
      let declared_output = Path.v "declared.txt" in
      let node =
        make_node
          ~package
          ~actions:[
            Riot_planner.Action.WriteFile { destination = Path.v "actual.txt"; content = "hello" };
          ]
          ~outs:[ declared_output ]
          ()
      in
      let completed = HashMap.create () in
      let result =
        Action_executor.execute_node
          ~completed
          ~store
          ~session_id:(Riot_model.Session_id.make ())
          ~build_target:test_build_target
          (test_toolchain ())
          sandbox
          node
      in
      match result.status with
      | Action_executor.Failed (OutputsNotCreated { missing }) ->
          let expected = Path.to_string Path.(sandbox / declared_output) in
          if List.any missing ~fn:(fun path -> String.equal (Path.to_string path) expected) then
            Ok ()
          else
            Error "expected missing declared output to be reported"
      | Action_executor.Failed _ -> Error "expected output verification failure"
      | Action_executor.Cached _
      | Action_executor.Executed _
      | Action_executor.Skipped -> Error "expected missing output failure") with
  | Ok result -> result
  | Error err -> Error ("tempdir creation failed: " ^ IO.error_message err)

let test_execute_node_skips_when_dependency_failed = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"action_executor_skip"
    (fun tmpdir ->
      let sandbox = Path.(tmpdir / Path.v "sandbox") in
      let _ =
        Fs.create_dir_all sandbox
        |> Result.expect ~msg:"create sandbox failed"
      in
      let workspace = make_workspace tmpdir in
      let store = Riot_store.Store.create ~workspace in
      let package = make_package ~root:tmpdir ~name:"pkg" in
      let dependency =
        make_node
          ~package
          ~actions:[
            Riot_planner.Action.WriteFile {
              destination = Path.v "dep.txt";
              content = "dependency";
            };
          ]
          ~outs:[ Path.v "dep.txt" ]
          ()
      in
      let node =
        make_node
          ~package
          ~dep_nodes:[ dependency ]
          ~actions:[
            Riot_planner.Action.WriteFile { destination = Path.v "out.txt"; content = "child" };
          ]
          ~outs:[ Path.v "out.txt" ]
          ()
      in
      let completed = HashMap.create () in
      let _ =
        HashMap.insert
          completed
          ~key:(node_id dependency)
          ~value:(failed_result (node_id dependency))
      in
      let result =
        Action_executor.execute_node
          ~completed
          ~store
          ~session_id:(Riot_model.Session_id.make ())
          ~build_target:test_build_target
          (test_toolchain ())
          sandbox
          node
      in
      match result.status with
      | Action_executor.Skipped -> Ok ()
      | Action_executor.Cached _
      | Action_executor.Executed _
      | Action_executor.Failed _ -> Error "expected node to be skipped") with
  | Ok result -> result
  | Error err -> Error ("tempdir creation failed: " ^ IO.error_message err)

let test_action_input_hash_tracks_dependency_output_hashes = fun _ctx ->
  let planned_hash = Crypto.hash_string "planned-action" in
  let first =
    Action_executor.compute_action_input_hash
      ~planned_hash
      ~dependency_output_hashes:[ Crypto.hash_string "dep-output-a" ]
  in
  let second =
    Action_executor.compute_action_input_hash
      ~planned_hash
      ~dependency_output_hashes:[ Crypto.hash_string "dep-output-b" ]
  in
  if Crypto.Hash.equal first second then
    Error "action input hash should change when dependency output hash changes"
  else
    Ok ()

let tests =
  Test.[
    case
      "action input hash tracks dependency output hashes"
      test_action_input_hash_tracks_dependency_output_hashes;
    case "execute_node writes declared output" test_execute_node_writes_file;
    case "execute_node copies file action outputs" test_execute_node_copies_file;
    case
      "execute_node fails when declared output is missing"
      test_execute_node_fails_when_declared_output_is_missing;
    case
      "execute_node skips when dependency already failed"
      test_execute_node_skips_when_dependency_failed;
  ]

let name = "riot-build:action-executor"

let main ~args = Test.Cli.main ~name ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
