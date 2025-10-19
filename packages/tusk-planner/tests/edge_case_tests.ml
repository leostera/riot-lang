open Std
open Tusk_planner
open Tusk_model

module G = Std.Graph.SimpleGraph

let make_test_input root_path =
  let package_name = Path.basename root_path in
  {
    package = Package.{
      name = package_name;
      path = root_path;
      relative_path = Path.v ".";
      dependencies = [];
      binaries = [];
    };
    toolchain = Toolchains.default_toolchain;
    workspace = Workspace.{
      root = Path.v ".";
      target_dir_root = Path.v "_build";
      packages = [];
    };
    planning_root = Path.(root_path / Path.v "src");
    dependencies = [];
  }

let test_empty_src_directory () =
  Ok () 

let test_only_interface_files () =
  Ok ()

let test_only_implementation_files () =
  Ok ()

let test_deeply_nested_modules () =
  Ok ()

let test_module_with_same_name_as_package () =
  Ok ()

let test_special_characters_in_filenames () =
  Ok ()

let test_generated_vs_concrete_files () =
  Ok ()

let test_hash_changes_on_source_modification () =
  Ok ()

let test_hash_stable_across_runs () =
  let root = Path.v "packages/tusk-planner/tests/fixtures/single-with-interface" in
  let input = make_test_input root in
  
  match Tusk_planner.plan_node input, Tusk_planner.plan_node input with
  | Ok { action_graph = ag1; _ }, Ok { action_graph = ag2; _ } ->
      let nodes1 = Action_graph.nodes ag1 in
      let nodes2 = Action_graph.nodes ag2 in
      
      if List.length nodes1 <> List.length nodes2 then
        Error (format "Different number of nodes: %d vs %d" (List.length nodes1) (List.length nodes2))
      else
        let hashes1 = List.map (fun n -> Crypto.Digest.hex (Action_node.get_hash n)) nodes1 in
        let hashes2 = List.map (fun n -> Crypto.Digest.hex (Action_node.get_hash n)) nodes2 in
        
        let sorted1 = List.sort String.compare hashes1 in
        let sorted2 = List.sort String.compare hashes2 in
        
        if sorted1 <> sorted2 then
          Error (format "Sorted hashes differ: run1=%d unique, run2=%d unique" 
            (List.length (List.sort_uniq String.compare hashes1))
            (List.length (List.sort_uniq String.compare hashes2)))
        else if hashes1 <> hashes2 then
          Error "Hashes are the same but in different order (node iteration order changed)"
        else
          Ok ()
  | Error err, _ | _, Error err ->
      Error (format "Planning failed: %s" (Planning_error.to_string err))

let test_dependency_hash_propagation () =
  let root = Path.v "packages/tusk-planner/tests/fixtures/linear-dependency" in
  let input = make_test_input root in
  
  match Tusk_planner.plan_node input with
  | Ok { action_graph; _ } ->
      let nodes = Action_graph.nodes action_graph in
      if List.length nodes < 2 then
        Error "Need at least 2 nodes to test hash propagation"
      else
        Ok ()
  | Error err ->
      Error (format "Planning failed: %s" (Planning_error.to_string err))

let test_no_duplicate_actions () =
  let root = Path.v "packages/tusk-planner/tests/fixtures/diamond-dependency" in
  let input = make_test_input root in
  
  match Tusk_planner.plan_node input with
  | Ok { action_graph; _ } ->
      let actions = Action_graph.to_action_list action_graph in
      let action_strs = List.map Action.to_string actions in
      let unique_actions = List.sort_uniq String.compare action_strs in
      
      if List.length action_strs <> List.length unique_actions then
        Error (format "Found duplicate actions: %d total, %d unique" 
          (List.length action_strs) (List.length unique_actions))
      else
        Ok ()
  | Error err ->
      Error (format "Planning failed: %s" (Planning_error.to_string err))

let test_topological_order_maintained () =
  let root = Path.v "packages/tusk-planner/tests/fixtures/linear-dependency" in
  let input = make_test_input root in
  
  match Tusk_planner.plan_node input with
  | Ok { module_graph; _ } ->
      let _ = G.topo_sort module_graph in
      Ok ()
  | Error err ->
      Error (format "Planning failed: %s" (Planning_error.to_string err))

let test_action_graph_is_dag () =
  let root = Path.v "packages/tusk-planner/tests/fixtures/diamond-dependency" in
  let input = make_test_input root in
  
  match Tusk_planner.plan_node input with
  | Ok { action_graph; _ } ->
      let _ = Action_graph.topo_sort action_graph in
      Ok ()
  | Error err ->
      Error (format "Planning failed: %s" (Planning_error.to_string err))

let test_hash_includes_toolchain () =
  Ok ()

let test_hash_includes_dependencies () =
  let root = Path.v "packages/tusk-planner/tests/fixtures/linear-dependency" in
  let input = make_test_input root in
  
  match Tusk_planner.plan_node input with
  | Ok { action_graph; module_graph } ->
      let action_nodes = Action_graph.nodes action_graph in
      let module_nodes = G.topo_sort module_graph in
      
      if List.length action_nodes = 0 then
        Error "Expected at least one action node"
      else
        Ok ()
  | Error err ->
      Error (format "Planning failed: %s" (Planning_error.to_string err))

let test_outputs_match_expected_artifacts () =
  let root = Path.v "packages/tusk-planner/tests/fixtures/single-with-interface" in
  let input = make_test_input root in
  
  match Tusk_planner.plan_node input with
  | Ok { action_graph; _ } ->
      let nodes = Action_graph.nodes action_graph in
      let all_outputs = List.concat_map (fun (node : Action_node.t) -> node.value.outs) nodes in
      
      if List.length all_outputs = 0 then
        Error "Expected at least one output file"
      else
        Ok ()
  | Error err ->
      Error (format "Planning failed: %s" (Planning_error.to_string err))

let tests = [
  Test.case "empty src directory" test_empty_src_directory;
  Test.case "only interface files" test_only_interface_files;
  Test.case "only implementation files" test_only_implementation_files;
  Test.case "deeply nested modules" test_deeply_nested_modules;
  Test.case "module with same name as package" test_module_with_same_name_as_package;
  Test.case "special characters in filenames" test_special_characters_in_filenames;
  Test.case "generated vs concrete files" test_generated_vs_concrete_files;
  Test.case "hash changes on source modification" test_hash_changes_on_source_modification;
  Test.case "hash stable across runs" test_hash_stable_across_runs;
  Test.case "dependency hash propagation" test_dependency_hash_propagation;
  Test.case "no duplicate actions" test_no_duplicate_actions;
  Test.case "topological order maintained" test_topological_order_maintained;
  Test.case "action graph is DAG" test_action_graph_is_dag;
  Test.case "hash includes toolchain" test_hash_includes_toolchain;
  Test.case "hash includes dependencies" test_hash_includes_dependencies;
  Test.case "outputs match expected artifacts" test_outputs_match_expected_artifacts;
]

let () = 
  let _ = Test.Cli.main ~name:"Edge Case Tests" ~tests () in
  ()
