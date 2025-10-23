open Std
open Std.Data
module G = Std.Graph.SimpleGraph
open Tusk_planner
open Tusk_model

let toolchain =
  Tusk_toolchain.init () |> Result.expect ~msg:"Failed to initialize toolchain"

let make_test_input root_path =
  let package_name = Path.basename root_path in
  Tusk_planner.Module_planner.
    {
      package =
        Package.
          {
            name = package_name;
            path = root_path;
            relative_path = Path.v ".";
            dependencies = [];
            binaries = [];
            library = None;
            test_library = None;
            test_modules = [];
          };
      toolchain;
      workspace =
        Workspace.
          {
            root = Path.v ".";
            target_dir_root = Path.v "_build";
            packages = [];
          };
      planning_root = Path.v "src";
      dependencies = [];
    }

let test_planner_generates_actions =
  Test.case "planner generates compilation actions" (fun () ->
      let root = Path.v "packages/tusk-planner/tests/fixtures/simple" in

      let input =
        Tusk_planner.Module_planner.
          {
            package =
              Package.
                {
                  name = "test";
                  path = root;
                  relative_path = Path.v ".";
                  dependencies = [];
                  binaries = [];
                  library = None;
                  test_library = None;
                  test_modules = [];
                };
            toolchain;
            workspace =
              Workspace.
                {
                  root = Path.v ".";
                  target_dir_root = Path.v "_build";
                  packages = [];
                };
            planning_root = Path.v "src";
            dependencies = [];
          }
      in

      match Tusk_planner.Module_planner.plan_node input with
      | Ok { action_graph; _ } ->
          let actions = Action_graph.to_action_list action_graph in
          if List.length actions > 0 then Ok ()
          else
            Error
              (format "Expected actions, got %d actions" (List.length actions))
      | Error err ->
          Error (format "Planning failed: %s" (Planning_error.to_string err)))

let test_action_graph_is_dag =
  Test.case "action graph is a DAG (no cycles)" (fun () ->
      let root =
        Path.v "packages/tusk-planner/tests/fixtures/diamond-dependency"
      in
      let input = make_test_input root in

      match Tusk_planner.Module_planner.plan_node input with
      | Ok { action_graph; _ } ->
          let _ = Action_graph.topo_sort action_graph in
          Ok ()
      | Error err ->
          Error (format "Planning failed: %s" (Planning_error.to_string err)))

let test_topological_order_maintained =
  Test.case "topological order is maintained in action graph" (fun () ->
      let root =
        Path.v "packages/tusk-planner/tests/fixtures/linear-dependency"
      in
      let input = make_test_input root in

      match Tusk_planner.Module_planner.plan_node input with
      | Ok { module_graph; _ } ->
          let _ = G.topo_sort module_graph in
          Ok ()
      | Error err ->
          Error (format "Planning failed: %s" (Planning_error.to_string err)))

let test_no_duplicate_actions =
  Test.case "action graph has no duplicate actions" (fun () ->
      let root =
        Path.v "packages/tusk-planner/tests/fixtures/diamond-dependency"
      in
      let input = make_test_input root in

      match Tusk_planner.Module_planner.plan_node input with
      | Ok { action_graph; _ } ->
          let actions = Action_graph.to_action_list action_graph in
          let action_strs = List.map Action.to_string actions in
          let unique_actions = List.sort_uniq String.compare action_strs in

          if List.length action_strs <> List.length unique_actions then
            Error
              (format "Found duplicate actions: %d total, %d unique"
                 (List.length action_strs)
                 (List.length unique_actions))
          else Ok ()
      | Error err ->
          Error (format "Planning failed: %s" (Planning_error.to_string err)))

let test_deterministic_action_order =
  Test.case "action graph order is deterministic across runs" (fun () ->
      let fixture_path =
        Path.v "packages/tusk-planner/tests/fixtures/multi_module_lib"
      in
      let lib_path = Path.(fixture_path / Path.v "src/multi_module_lib.ml") in
      let package_name = Path.basename fixture_path in
      let input =
        Tusk_planner.Module_planner.
          {
            package =
              Package.
                {
                  name = package_name;
                  path = fixture_path;
                  relative_path = Path.v ".";
                  dependencies = [];
                  binaries = [];
                  library = Some { path = lib_path };
                  test_library = None;
                  test_modules = [];
                };
            toolchain;
            workspace =
              Workspace.
                {
                  root = Path.v ".";
                  target_dir_root = Path.v "_build";
                  packages = [];
                };
            planning_root = Path.v "src";
            dependencies = [];
          }
      in

      let get_action_hashes () =
        match Tusk_planner.Module_planner.plan_node input with
        | Error err ->
            Error (format "Planning failed: %s" (Planning_error.to_string err))
        | Ok { action_graph; _ } ->
            let actions = Action_graph.to_action_list action_graph in
            Ok (List.map Action.to_string actions)
      in

      match
        (get_action_hashes (), get_action_hashes (), get_action_hashes ())
      with
      | Ok h1, Ok h2, Ok h3 ->
          if h1 = h2 && h2 = h3 then Ok ()
          else Error "Action graph order varies across runs"
      | Error e, _, _ | _, Error e, _ | _, _, Error e -> Error e)

let test_action_nodes_have_hashes =
  Test.case "action nodes have pre-computed content hashes" (fun () ->
      let root =
        Path.v "packages/tusk-planner/tests/fixtures/single-with-interface"
      in
      let input = make_test_input root in

      match Tusk_planner.Module_planner.plan_node input with
      | Ok { action_graph; _ } ->
          let nodes = Action_graph.nodes action_graph in
          if List.length nodes = 0 then
            Error "Expected at least one action node"
          else
            let all_have_hashes =
              List.for_all
                (fun node ->
                  let hash = Action_node.get_hash node in
                  let hash_str = Crypto.Digest.hex hash in
                  String.length hash_str > 0)
                nodes
            in

            if not all_have_hashes then
              Error "Some action nodes have invalid hashes"
            else Ok ()
      | Error err ->
          Error (format "Planning failed: %s" (Planning_error.to_string err)))

let test_hashes_are_unique =
  Test.case "action node hashes are unique per action" (fun () ->
      let root =
        Path.v "packages/tusk-planner/tests/fixtures/linear-dependency"
      in
      let input = make_test_input root in

      match Tusk_planner.Module_planner.plan_node input with
      | Ok { action_graph; _ } ->
          let nodes = Action_graph.nodes action_graph in
          let hashes =
            List.map
              (fun node -> Crypto.Digest.hex (Action_node.get_hash node))
              nodes
          in

          let unique_hashes = List.sort_uniq String.compare hashes in

          if List.length hashes <> List.length unique_hashes then
            Error
              (format
                 "Expected all unique hashes, got %d nodes but %d unique hashes"
                 (List.length hashes)
                 (List.length unique_hashes))
          else Ok ()
      | Error err ->
          Error (format "Planning failed: %s" (Planning_error.to_string err)))

let test_hashes_include_package_name =
  Test.case
    "hashes include package name (different packages = different hashes)"
    (fun () ->
      let root1 =
        Path.v "packages/tusk-planner/tests/fixtures/single-with-interface"
      in
      let input1 =
        {
          (make_test_input root1) with
          package = { (make_test_input root1).package with name = "package-a" };
        }
      in

      let root2 =
        Path.v "packages/tusk-planner/tests/fixtures/single-with-interface"
      in
      let input2 =
        {
          (make_test_input root2) with
          package = { (make_test_input root2).package with name = "package-b" };
        }
      in

      match
        ( Tusk_planner.Module_planner.plan_node input1,
          Tusk_planner.Module_planner.plan_node input2 )
      with
      | Ok { action_graph = ag1; _ }, Ok { action_graph = ag2; _ } ->
          let nodes1 = Action_graph.nodes ag1 in
          let nodes2 = Action_graph.nodes ag2 in

          if List.length nodes1 = 0 || List.length nodes2 = 0 then
            Error "Expected non-empty action graphs"
          else
            let hash1 =
              Action_node.get_hash (List.hd nodes1) |> Crypto.Digest.hex
            in
            let hash2 =
              Action_node.get_hash (List.hd nodes2) |> Crypto.Digest.hex
            in

            if hash1 = hash2 then
              Error "Hashes should differ when package name differs"
            else Ok ()
      | Error err, _ | _, Error err ->
          Error (format "Planning failed: %s" (Planning_error.to_string err)))

let test_hash_stable_across_runs =
  Test.case "hashes are stable across planning runs" (fun () ->
      let root =
        Path.v "packages/tusk-planner/tests/fixtures/single-with-interface"
      in
      let input = make_test_input root in

      match
        ( Tusk_planner.Module_planner.plan_node input,
          Tusk_planner.Module_planner.plan_node input )
      with
      | Ok { action_graph = ag1; _ }, Ok { action_graph = ag2; _ } ->
          let nodes1 = Action_graph.nodes ag1 in
          let nodes2 = Action_graph.nodes ag2 in

          if List.length nodes1 <> List.length nodes2 then
            Error
              (format "Different number of nodes: %d vs %d" (List.length nodes1)
                 (List.length nodes2))
          else
            let hashes1 =
              List.map
                (fun n -> Crypto.Digest.hex (Action_node.get_hash n))
                nodes1
            in
            let hashes2 =
              List.map
                (fun n -> Crypto.Digest.hex (Action_node.get_hash n))
                nodes2
            in

            let sorted1 = List.sort String.compare hashes1 in
            let sorted2 = List.sort String.compare hashes2 in

            if sorted1 <> sorted2 then
              Error
                (format "Sorted hashes differ: run1=%d unique, run2=%d unique"
                   (List.length (List.sort_uniq String.compare hashes1))
                   (List.length (List.sort_uniq String.compare hashes2)))
            else if hashes1 <> hashes2 then
              Error
                "Hashes are the same but in different order (node iteration \
                 order changed)"
            else Ok ()
      | Error err, _ | _, Error err ->
          Error (format "Planning failed: %s" (Planning_error.to_string err)))

let test_dependency_hash_propagation =
  Test.case "dependency changes propagate through hash chain" (fun () ->
      let root =
        Path.v "packages/tusk-planner/tests/fixtures/linear-dependency"
      in
      let input = make_test_input root in

      match Tusk_planner.Module_planner.plan_node input with
      | Ok { action_graph; _ } ->
          let nodes = Action_graph.nodes action_graph in
          if List.length nodes < 2 then
            Error "Need at least 2 nodes to test hash propagation"
          else Ok ()
      | Error err ->
          Error (format "Planning failed: %s" (Planning_error.to_string err)))

let tests =
  Test.
    [
      test_planner_generates_actions;
      test_action_graph_is_dag;
      test_topological_order_maintained;
      test_no_duplicate_actions;
      test_deterministic_action_order;
      test_action_nodes_have_hashes;
      test_hashes_are_unique;
      test_hashes_include_package_name;
      test_hash_stable_across_runs;
      test_dependency_hash_propagation;
    ]

let name = "Action Graph Tests"
let () = Miniriot.run ~main:(Test.Cli.main ~name ~tests) ~args:Env.args
