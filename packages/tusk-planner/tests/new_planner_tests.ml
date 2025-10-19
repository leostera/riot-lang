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

let make_test_input_with_binaries root_path binaries =
  let package_name = Path.basename root_path in
  {
    package = Package.{
      name = package_name;
      path = root_path;
      relative_path = Path.v ".";
      dependencies = [];
      binaries;
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

let test_single_module_with_interface () =
  let root = Path.v "packages/tusk-planner/tests/fixtures/single-with-interface" in
  let input = make_test_input root in
  
  match Tusk_planner.plan_node input with
  | Ok { module_graph; action_graph } ->
      let actions = Action_graph.to_action_list action_graph in
      let module_nodes = G.topo_sort module_graph in
      
      if List.length module_nodes = 0 then
        Error "Expected at least one module node"
      else if List.length actions = 0 then
        Error "Expected at least one action"
      else
        Ok ()
  | Error err ->
      Error (format "Planning failed: %s" (Planning_error.to_string err))

let test_linear_dependency () =
  let root = Path.v "packages/tusk-planner/tests/fixtures/linear-dependency" in
  let input = make_test_input root in
  
  match Tusk_planner.plan_node input with
  | Ok { module_graph; action_graph } ->
      let module_nodes = G.topo_sort module_graph in
      let actions = Action_graph.to_action_list action_graph in
      
      if List.length module_nodes = 0 then
        Error "Expected at least one module node"
      else if List.length actions = 0 then
        Error "Expected at least one action"
      else
        Ok ()
  | Error err ->
      Error (format "Planning failed: %s" (Planning_error.to_string err))

let test_diamond_dependency () =
  let root = Path.v "packages/tusk-planner/tests/fixtures/diamond-dependency" in
  let input = make_test_input root in
  
  match Tusk_planner.plan_node input with
  | Ok { module_graph; _ } ->
      let nodes = G.topo_sort module_graph in
      if List.length nodes = 0 then
        Error "Expected at least one module node"
      else
        Ok ()
  | Error err ->
      Error (format "Planning failed: %s" (Planning_error.to_string err))

let test_c_stubs () =
  let root = Path.v "packages/tusk-planner/tests/fixtures/c-stubs" in
  let input = make_test_input root in
  
  match Tusk_planner.plan_node input with
  | Ok { module_graph; action_graph } ->
      let module_nodes = G.topo_sort module_graph in
      let c_nodes = List.filter (fun (node : Module_node.t G.node) ->
        match node.value.kind with
        | Module_node.C -> true
        | _ -> false
      ) module_nodes in
      
      let actions = Action_graph.to_action_list action_graph in
      let has_compile_c = List.exists (function
        | Action.CompileC _ -> true
        | _ -> false) actions in
      
      if List.length c_nodes = 0 then
        Error "No C nodes found in module graph - C files not being scanned"
      else if not has_compile_c then
        Error (format "Found %d C node(s) in module graph but no CompileC action generated" (List.length c_nodes))
      else
        Ok ()
  | Error err ->
      Error (format "Planning failed: %s" (Planning_error.to_string err))

let test_mixed_interfaces () =
  let root = Path.v "packages/tusk-planner/tests/fixtures/mixed-interfaces" in
  let input = make_test_input root in
  
  match Tusk_planner.plan_node input with
  | Ok { module_graph; _ } ->
      let nodes = G.topo_sort module_graph in
      if List.length nodes = 0 then
        Error "Expected at least one node"
      else
        Ok ()
  | Error err ->
      Error (format "Planning failed: %s" (Planning_error.to_string err))

let test_library_with_binary () =
  let root = Path.v "packages/tusk-planner/tests/fixtures/library-with-binary" in
  let binaries = [Package.{ name = "main"; path = Path.v "bin/main.ml" }] in
  let input = make_test_input_with_binaries root binaries in
  
  match Tusk_planner.plan_node input with
  | Ok { action_graph; _ } ->
      let actions = Action_graph.to_action_list action_graph in
      let has_exe = List.exists (function
        | Action.CreateExecutable _ -> true
        | _ -> false) actions in
      
      if not has_exe then
        Error "Expected CreateExecutable action for binary"
      else
        Ok ()
  | Error err ->
      Error (format "Planning failed: %s" (Planning_error.to_string err))

let test_multiple_binaries () =
  let root = Path.v "packages/tusk-planner/tests/fixtures/multiple-binaries" in
  let binaries = [
    Package.{ name = "cli"; path = Path.v "bin/cli.ml" };
    Package.{ name = "server"; path = Path.v "bin/server.ml" };
  ] in
  let input = make_test_input_with_binaries root binaries in
  
  match Tusk_planner.plan_node input with
  | Ok { action_graph; _ } ->
      let actions = Action_graph.to_action_list action_graph in
      let exe_count = List.fold_left (fun count action ->
        match action with
        | Action.CreateExecutable _ -> count + 1
        | _ -> count
      ) 0 actions in
      
      if exe_count < 2 then
        Error (format "Expected 2 CreateExecutable actions, got %d" exe_count)
      else
        Ok ()
  | Error err ->
      Error (format "Planning failed: %s" (Planning_error.to_string err))

let test_circular_dependency () =
  let root = Path.v "packages/tusk-planner/tests/fixtures/circular-dependency" in
  let input = make_test_input root in
  
  match Tusk_planner.plan_node input with
  | Ok _ ->
      Error "Expected CyclicDependency error for circular dependency"
  | Error (Planning_error.CyclicDependency { cycle }) ->
      if List.length cycle > 0 then Ok ()
      else Error "Cycle detected but cycle list is empty"
  | Error err ->
      Error (format "Expected CyclicDependency, got: %s" (Planning_error.to_string err))

let test_action_nodes_have_hashes () =
  let root = Path.v "packages/tusk-planner/tests/fixtures/single-with-interface" in
  let input = make_test_input root in
  
  match Tusk_planner.plan_node input with
  | Ok { action_graph; _ } ->
      let nodes = Action_graph.nodes action_graph in
      if List.length nodes = 0 then
        Error "Expected at least one action node"
      else
        let all_have_hashes = List.for_all (fun node ->
          let hash = Action_node.get_hash node in
          let hash_str = Crypto.Digest.hex hash in
          String.length hash_str > 0
        ) nodes in
        
        if not all_have_hashes then
          Error "Some action nodes have invalid hashes"
        else
          Ok ()
  | Error err ->
      Error (format "Planning failed: %s" (Planning_error.to_string err))

let test_hashes_include_package_name () =
  let root1 = Path.v "packages/tusk-planner/tests/fixtures/single-with-interface" in
  let input1 = { (make_test_input root1) with
    package = { (make_test_input root1).package with name = "package-a" }
  } in
  
  let root2 = Path.v "packages/tusk-planner/tests/fixtures/single-with-interface" in  
  let input2 = { (make_test_input root2) with
    package = { (make_test_input root2).package with name = "package-b" }
  } in
  
  match Tusk_planner.plan_node input1, Tusk_planner.plan_node input2 with
  | Ok { action_graph = ag1; _ }, Ok { action_graph = ag2; _ } ->
      let nodes1 = Action_graph.nodes ag1 in
      let nodes2 = Action_graph.nodes ag2 in
      
      if List.length nodes1 = 0 || List.length nodes2 = 0 then
        Error "Expected non-empty action graphs"
      else
        let hash1 = Action_node.get_hash (List.hd nodes1) |> Crypto.Digest.hex in
        let hash2 = Action_node.get_hash (List.hd nodes2) |> Crypto.Digest.hex in
        
        if hash1 = hash2 then
          Error "Hashes should differ when package name differs"
        else
          Ok ()
  | Error err, _ | _, Error err ->
      Error (format "Planning failed: %s" (Planning_error.to_string err))

let test_hashes_are_unique () =
  let root = Path.v "packages/tusk-planner/tests/fixtures/linear-dependency" in
  let input = make_test_input root in
  
  match Tusk_planner.plan_node input with
  | Ok { action_graph; _ } ->
      let nodes = Action_graph.nodes action_graph in
      let hashes = List.map (fun node ->
        Crypto.Digest.hex (Action_node.get_hash node)
      ) nodes in
      
      let unique_hashes = List.sort_uniq String.compare hashes in
      
      if List.length hashes <> List.length unique_hashes then
        Error (format "Expected all unique hashes, got %d nodes but %d unique hashes" 
          (List.length hashes) (List.length unique_hashes))
      else
        Ok ()
  | Error err ->
      Error (format "Planning failed: %s" (Planning_error.to_string err))

let tests = [
  Test.case "single module with interface" test_single_module_with_interface;
  Test.case "linear dependency" test_linear_dependency;
  Test.case "diamond dependency" test_diamond_dependency;
  Test.case "c stubs" test_c_stubs;
  Test.case "mixed interfaces" test_mixed_interfaces;
  Test.case "library with binary" test_library_with_binary;
  Test.case "multiple binaries" test_multiple_binaries;
  Test.case "circular dependency detection" test_circular_dependency;
  Test.case "action nodes have pre-computed hashes" test_action_nodes_have_hashes;
  Test.case "hashes include package name" test_hashes_include_package_name;
  Test.case "hashes are unique per node" test_hashes_are_unique;
]

let () = 
  let _ = Test.Cli.main ~name:"Planner Tests" ~tests () in
  ()
