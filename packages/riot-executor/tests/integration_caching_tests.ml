open Std
open Std.Collections
module Test = Std.Test

let test_toolchain = fun () ->
  Riot_toolchain.init ~config:Riot_model.Toolchain_config.default
  |> Result.expect ~msg:"failed to initialize toolchain"

let make_workspace = fun root ->
  Riot_model.Workspace.{
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

let make_package = fun ~root ~name ->
  let path = Path.(root / Path.v "packages" / Path.v name) in
  Riot_model.Package.make
    ~name
    ~path
    ~relative_path:(Path.v ("packages/" ^ name))
    ~library:{ path = Path.v "src/lib.ml" }
    ()

let make_graph_with_write = fun ~package ~content ->
  let graph = Riot_planner.Action_graph.create () in
  let spec =
    Riot_planner.Action_node.make
      ~actions:[ Riot_planner.Action.WriteFile {
        destination = Path.v "out.txt";
        content
      }; ]
      ~outs:[ Path.v "out.txt" ]
      ~srcs:[]
      ~package
      ~toolchain:(test_toolchain ())
      ~dependency_hashes:(fun _ -> Crypto.hash_string "")
      ~deps:[]
  in
  let node = Riot_planner.Action_graph.add_node graph spec in
  (graph, node)

let node_id = fun (node: Riot_planner.Action_node.t) -> node.id

let execute_graph = fun ~workspace ~store ~package ~graph ->
  let sandbox = Riot_executor.Sandbox.create ~workspace () ~package_name:package.Riot_model.Package.name in
  let result = Riot_executor.Action_executor.execute
    ~action_graph:graph
    ~sandbox
    ~store
    ~session_id:(Riot_model.Session_id.make ())
    (test_toolchain ())
    ~concurrency:1
  in
  let output = Path.(Riot_executor.Sandbox.get_dir sandbox / Path.v "out.txt") in
  let output_content = Fs.read_to_string output in
  let output_exists = Fs.exists output |> Result.unwrap_or ~default:false in
  let sandbox_dir = Riot_executor.Sandbox.get_dir sandbox in
  let _ = Riot_executor.Sandbox.cleanup sandbox in
  (result, output_exists, output_content, sandbox_dir)

let test_execute_reuses_cache_for_equivalent_graph = fun _ctx ->
  match
    Fs.with_tempdir ~prefix:"integration_cache_equivalent"
      (fun tmpdir ->
        let workspace = make_workspace tmpdir in
        let store = Riot_store.Store.create ~workspace in
        let package = make_package ~root:tmpdir ~name:"pkg" in
        let graph1, node1 = make_graph_with_write ~package ~content:"cached output" in
        let result1, exists1, content1, _sandbox1 = execute_graph ~workspace ~store ~package ~graph:graph1 in
        let graph2, node2 = make_graph_with_write ~package ~content:"cached output" in
        let result2, exists2, content2, _sandbox2 = execute_graph ~workspace ~store ~package ~graph:graph2 in
        match
          ( HashMap.get result1.Riot_executor.Action_executor.completed ~key:(node_id node1),
            HashMap.get result2.Riot_executor.Action_executor.completed ~key:(node_id node2),
            content1,
            content2 )
        with
        | Some { status = Riot_executor.Action_executor.Executed; _ },
          Some { status = Riot_executor.Action_executor.Cached _; _ },
          Ok first_content,
          Ok second_content ->
            if exists1 && exists2 && String.equal first_content "cached output" && String.equal second_content "cached output" then
              Ok ()
            else
              Error "expected both sandboxes to materialize identical cached output"
        | _ -> Error "expected first run executed and second run cached")
  with
  | Ok result -> result
  | Error err -> Error ("tempdir creation failed: " ^ IO.error_message err)

let test_execute_cache_misses_when_action_changes = fun _ctx ->
  match
    Fs.with_tempdir ~prefix:"integration_cache_changed"
      (fun tmpdir ->
        let workspace = make_workspace tmpdir in
        let store = Riot_store.Store.create ~workspace in
        let package = make_package ~root:tmpdir ~name:"pkg" in
        let graph1, node1 = make_graph_with_write ~package ~content:"v1" in
        let result1, exists1, _, _sandbox1 = execute_graph ~workspace ~store ~package ~graph:graph1 in
        let graph2, node2 = make_graph_with_write ~package ~content:"v2" in
        let result2, exists2, content2, _sandbox2 = execute_graph ~workspace ~store ~package ~graph:graph2 in
        match
          ( HashMap.get result1.Riot_executor.Action_executor.completed ~key:(node_id node1),
            HashMap.get result2.Riot_executor.Action_executor.completed ~key:(node_id node2),
            content2 )
        with
        | Some { status = Riot_executor.Action_executor.Executed; _ },
          Some { status = Riot_executor.Action_executor.Executed; _ },
          Ok second_content ->
            if exists1 && exists2 && String.equal second_content "v2" then
              Ok ()
            else
              Error "expected changed action to execute freshly with new output"
        | _ -> Error "expected changed action to miss cache")
  with
  | Ok result -> result
  | Error err -> Error ("tempdir creation failed: " ^ IO.error_message err)

let tests =
  Test.[
    case "equivalent action graph reuses cached artifact" test_execute_reuses_cache_for_equivalent_graph;
    case "changed action graph misses cache" test_execute_cache_misses_when_action_changes;
  ]

let name = "riot-executor:integration-caching"

let () = Actors.run ~main:(Test.Cli.main ~name ~tests) ~args:Env.args ()
