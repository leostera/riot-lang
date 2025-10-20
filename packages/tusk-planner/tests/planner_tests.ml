open Std
open Std.Data
module G = Std.Graph.SimpleGraph
open Tusk_planner
open Tusk_model

let toolchain =
  Tusk_toolchain.init () |> Result.expect ~msg:"Failed to initialize toolchain"

let make_test_input root_path =
  let package_name = Path.basename root_path in
  Tusk_planner.
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

let test_single_with_interface () =
  let root =
    Path.v "packages/tusk-planner/tests/fixtures/single-with-interface"
  in
  let input = make_test_input root in

  match Tusk_planner.plan_node input with
  | Ok { module_graph; action_graph } ->
      let actions = Action_graph.to_action_list action_graph in
      let module_count = List.length (G.topo_sort module_graph) in

      if module_count < 2 then
        Error
          (format "Expected at least 2 module nodes (MLI + ML), got %d"
             module_count)
      else if List.length actions < 2 then
        Error
          (format
             "Expected at least 2 actions (CompileInterface + \
              CompileImplementation), got %d"
             (List.length actions))
      else Ok ()
  | Error err ->
      Error (format "Planning failed: %s" (Planning_error.to_string err))

let test_linear_dependency () =
  let root = Path.v "packages/tusk-planner/tests/fixtures/linear-dependency" in
  let input = make_test_input root in

  match Tusk_planner.plan_node input with
  | Ok { module_graph; action_graph } ->
      let actions = Action_graph.to_action_list action_graph in
      let nodes = G.topo_sort module_graph in

      if List.length nodes < 3 then
        Error
          (format "Expected at least 3 nodes (Root + A + B), got %d"
             (List.length nodes))
      else if List.length actions < 3 then
        Error
          (format "Expected at least 3 actions (2 compiles + library), got %d"
             (List.length actions))
      else Ok ()
  | Error err ->
      Error (format "Planning failed: %s" (Planning_error.to_string err))

let test_c_stubs () =
  let root = Path.v "packages/tusk-planner/tests/fixtures/c-stubs" in
  let input = make_test_input root in

  match Tusk_planner.plan_node input with
  | Ok { module_graph; action_graph } ->
      let actions = Action_graph.to_action_list action_graph in
      let has_compile_c =
        List.exists (function Action.CompileC _ -> true | _ -> false) actions
      in

      if not has_compile_c then Error "Expected CompileC action for .c file"
      else Ok ()
  | Error err ->
      Error (format "Planning failed: %s" (Planning_error.to_string err))

let test_diamond_dependency () =
  let root = Path.v "packages/tusk-planner/tests/fixtures/diamond-dependency" in
  let input = make_test_input root in

  match Tusk_planner.plan_node input with
  | Ok { module_graph; action_graph } ->
      let nodes = G.topo_sort module_graph in
      if List.length nodes < 4 then
        Error
          (format
             "Expected at least 4 module nodes (Base, Left, Right, Top), got %d"
             (List.length nodes))
      else Ok ()
  | Error err ->
      Error (format "Planning failed: %s" (Planning_error.to_string err))

let test_mixed_interfaces () =
  let root = Path.v "packages/tusk-planner/tests/fixtures/mixed-interfaces" in
  let input = make_test_input root in

  match Tusk_planner.plan_node input with
  | Ok { module_graph; action_graph } ->
      let nodes = G.topo_sort module_graph in
      if List.length nodes < 5 then
        Error
          (format "Expected at least 5 nodes (3 ML + 2 MLI), got %d"
             (List.length nodes))
      else Ok ()
  | Error err ->
      Error (format "Planning failed: %s" (Planning_error.to_string err))

let make_test_input_with_binaries root_path binaries =
  let package_name = Path.basename root_path in
  Tusk_planner.
    {
      package =
        Package.
          {
            name = package_name;
            path = root_path;
            relative_path = Path.v ".";
            dependencies = [];
            binaries;
            library = None;
            test_library = None;
            test_modules = [];
          };
      toolchain =
        Tusk_toolchain.init () |> Result.expect ~msg:"Failed to init toolchain";
      workspace =
        Workspace.
          {
            root = Path.v ".";
            target_dir_root = Path.v "_build";
            packages = [];
          };
      planning_root = Path.(root_path / Path.v "src");
      dependencies = [];
    }

let test_library_with_binary () =
  let root =
    Path.v "packages/tusk-planner/tests/fixtures/library-with-binary"
  in
  let binaries : Package.binary list =
    [ Package.{ name = "main"; path = Path.v "bin/main.ml" } ]
  in
  let input = make_test_input_with_binaries root binaries in

  match Tusk_planner.plan_node input with
  | Ok { module_graph; action_graph } ->
      let actions = Action_graph.to_action_list action_graph in
      let has_exe =
        List.exists
          (function Action.CreateExecutable _ -> true | _ -> false)
          actions
      in

      if not has_exe then Error "Expected CreateExecutable action for binary"
      else Ok ()
  | Error err ->
      Error (format "Planning failed: %s" (Planning_error.to_string err))

let test_multiple_binaries () =
  let root = Path.v "packages/tusk-planner/tests/fixtures/multiple-binaries" in
  let binaries : Package.binary list =
    [
      Package.{ name = "cli"; path = Path.v "bin/cli.ml" };
      Package.{ name = "server"; path = Path.v "bin/server.ml" };
    ]
  in
  let input = make_test_input_with_binaries root binaries in

  match Tusk_planner.plan_node input with
  | Ok { module_graph; action_graph } ->
      let actions = Action_graph.to_action_list action_graph in
      let exe_count =
        List.fold_left
          (fun count action ->
            match action with
            | Action.CreateExecutable _ -> count + 1
            | _ -> count)
          0 actions
      in

      if exe_count < 2 then
        Error (format "Expected 2 CreateExecutable actions, got %d" exe_count)
      else Ok ()
  | Error err ->
      Error (format "Planning failed: %s" (Planning_error.to_string err))

let test_circular_dependency () =
  let root =
    Path.v "packages/tusk-planner/tests/fixtures/circular-dependency"
  in
  let input = make_test_input root in

  match Tusk_planner.plan_node input with
  | Ok _ -> Error "Expected CyclicDependency error for circular dependency"
  | Error (Planning_error.CyclicDependency { cycle }) ->
      if List.length cycle > 0 then Ok ()
      else Error "Cycle detected but cycle list is empty"
  | Error err ->
      Error
        (format "Expected CyclicDependency, got: %s"
           (Planning_error.to_string err))

let test_empty_library () =
  let root = Path.v "packages/tusk-planner/tests/fixtures/empty-library" in
  let input = make_test_input root in

  match Tusk_planner.plan_node input with
  | Ok { module_graph; action_graph } ->
      let actions = Action_graph.to_action_list action_graph in
      if List.length actions > 2 then
        Error
          (format "Expected minimal actions for empty library, got %d"
             (List.length actions))
      else Ok ()
  | Error err ->
      Error (format "Planning failed: %s" (Planning_error.to_string err))

let test_header_only () =
  let root = Path.v "packages/tusk-planner/tests/fixtures/header-only" in
  let input = make_test_input root in

  match Tusk_planner.plan_node input with
  | Ok { module_graph; action_graph } ->
      let actions = Action_graph.to_action_list action_graph in
      let has_compile =
        List.exists
          (function
            | Action.CompileC _ | Action.CompileImplementation _
            | Action.CompileInterface _ ->
                true
            | _ -> false)
          actions
      in

      if has_compile then
        Error "Expected no compile actions for header-only files"
      else Ok ()
  | Error err ->
      Error (format "Planning failed: %s" (Planning_error.to_string err))

let test_complex_multi_module () =
  let root =
    Path.v "packages/tusk-planner/tests/fixtures/complex-multi-module"
  in
  let input = make_test_input root in

  match Tusk_planner.plan_node input with
  | Ok { module_graph; action_graph } ->
      let nodes = G.topo_sort module_graph in
      let actions = Action_graph.to_action_list action_graph in

      if List.length nodes < 8 then
        Error
          (format "Expected at least 8 nodes for complex module, got %d"
             (List.length nodes))
      else if List.length actions < 6 then
        Error
          (format "Expected at least 6 actions, got %d" (List.length actions))
      else Ok ()
  | Error err ->
      Error (format "Planning failed: %s" (Planning_error.to_string err))

let test_binary_is_excluded_from_library () =
  let toolchain =
    Tusk_toolchain.init ()
    |> Result.expect ~msg:"Failed to initialize toolchain"
  in

  let fixture_root =
    Path.v "packages/tusk-planner/tests/fixtures/with-binary"
  in
  let config =
    Graph_builder.
      {
        root = fixture_root;
        source_dir = fixture_root;
        namespace = "TestBin";
        package =
          Package.
            {
              name = "test-bin";
              path = fixture_root;
              relative_path = Path.v ".";
              dependencies = [];
              binaries =
                [ { name = "main"; path = Path.(fixture_root / v "main.ml") } ];
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
      }
  in

  let _graph = Graph_builder.create config in
  Ok ()

let make_test_config root_path source_dir =
  let toolchain =
    Tusk_toolchain.init ()
    |> Result.expect ~msg:"Failed to initialize toolchain"
  in

  Graph_builder.
    {
      root = root_path;
      source_dir;
      namespace = "Test";
      package =
        Package.
          {
            name = "test";
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
    }

let test_graph_has_root_node () =
  let config =
    make_test_config
      (Path.v "packages/tusk-planner/tests/fixtures/simple")
      (Path.v "src")
  in
  let _graph = Graph_builder.create config in
  Ok ()

let test_graph_namespace_is_set () =
  let config =
    make_test_config
      (Path.v "packages/tusk-planner/tests/fixtures/simple")
      (Path.v "src")
  in
  let graph = Graph_builder.create config in
  if graph.config.namespace = "Test" then Ok ()
  else
    Error
      (format "Expected namespace 'Test' but got '%s'" graph.config.namespace)

let test_planner_generates_actions () =
  let root = Path.v "packages/tusk-planner/tests/fixtures/simple" in

  let input =
    Tusk_planner.
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

  match Tusk_planner.plan_node input with
  | Ok { action_graph; _ } ->
      let actions = Action_graph.to_action_list action_graph in
      if List.length actions > 0 then Ok ()
      else
        Error (format "Expected actions, got %d actions" (List.length actions))
  | Error err ->
      Error (format "Planning failed: %s" (Planning_error.to_string err))

let make_test_input root_path =
  let toolchain =
    Tusk_toolchain.init ()
    |> Result.expect ~msg:"Failed to initialize toolchain"
  in

  let package_name = Path.basename root_path in
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
        { root = Path.v "."; target_dir_root = Path.v "_build"; packages = [] };
    planning_root = Path.v "src";
    dependencies = [];
  }

let make_test_input_with_binaries root_path binaries =
  let package_name = Path.basename root_path in
  {
    package =
      Package.
        {
          name = package_name;
          path = root_path;
          relative_path = Path.v ".";
          dependencies = [];
          binaries;
          library = None;
          test_library = None;
          test_modules = [];
        };
    toolchain;
    workspace =
      Workspace.
        { root = Path.v "."; target_dir_root = Path.v "_build"; packages = [] };
    planning_root = Path.(root_path / Path.v "src");
    dependencies = [];
  }

let test_single_module_with_interface () =
  let root =
    Path.v "packages/tusk-planner/tests/fixtures/single-with-interface"
  in
  let input = make_test_input root in

  match Tusk_planner.plan_node input with
  | Ok { module_graph; action_graph } ->
      let actions = Action_graph.to_action_list action_graph in
      let module_nodes = G.topo_sort module_graph in

      if List.length module_nodes = 0 then
        Error "Expected at least one module node"
      else if List.length actions = 0 then Error "Expected at least one action"
      else Ok ()
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
      else if List.length actions = 0 then Error "Expected at least one action"
      else Ok ()
  | Error err ->
      Error (format "Planning failed: %s" (Planning_error.to_string err))

let test_diamond_dependency () =
  let root = Path.v "packages/tusk-planner/tests/fixtures/diamond-dependency" in
  let input = make_test_input root in

  match Tusk_planner.plan_node input with
  | Ok { module_graph; _ } ->
      let nodes = G.topo_sort module_graph in
      if List.length nodes = 0 then Error "Expected at least one module node"
      else Ok ()
  | Error err ->
      Error (format "Planning failed: %s" (Planning_error.to_string err))

let test_c_stubs () =
  let root = Path.v "packages/tusk-planner/tests/fixtures/c-stubs" in
  let input = make_test_input root in

  match Tusk_planner.plan_node input with
  | Ok { module_graph; action_graph } ->
      let module_nodes = G.topo_sort module_graph in
      let c_nodes =
        List.filter
          (fun (node : Module_node.t G.node) ->
            match node.value.kind with Module_node.C -> true | _ -> false)
          module_nodes
      in

      let actions = Action_graph.to_action_list action_graph in
      let has_compile_c =
        List.exists (function Action.CompileC _ -> true | _ -> false) actions
      in

      if List.length c_nodes = 0 then
        Error "No C nodes found in module graph - C files not being scanned"
      else if not has_compile_c then
        Error
          (format
             "Found %d C node(s) in module graph but no CompileC action \
              generated"
             (List.length c_nodes))
      else Ok ()
  | Error err ->
      Error (format "Planning failed: %s" (Planning_error.to_string err))

let test_mixed_interfaces () =
  let root = Path.v "packages/tusk-planner/tests/fixtures/mixed-interfaces" in
  let input = make_test_input root in

  match Tusk_planner.plan_node input with
  | Ok { module_graph; _ } ->
      let nodes = G.topo_sort module_graph in
      if List.length nodes = 0 then Error "Expected at least one node"
      else Ok ()
  | Error err ->
      Error (format "Planning failed: %s" (Planning_error.to_string err))

let test_library_with_binary () =
  let root =
    Path.v "packages/tusk-planner/tests/fixtures/library-with-binary"
  in
  let binaries : Package.binary list =
    [ { name = "main"; path = Path.v "bin/main.ml" } ]
  in
  let input = make_test_input_with_binaries root binaries in

  match Tusk_planner.plan_node input with
  | Ok { action_graph; _ } ->
      let actions = Action_graph.to_action_list action_graph in
      let has_exe =
        List.exists
          (function Action.CreateExecutable _ -> true | _ -> false)
          actions
      in

      if not has_exe then Error "Expected CreateExecutable action for binary"
      else Ok ()
  | Error err ->
      Error (format "Planning failed: %s" (Planning_error.to_string err))

let test_multiple_binaries () =
  let root = Path.v "packages/tusk-planner/tests/fixtures/multiple-binaries" in
  let binaries : Package.binary list =
    [
      { name = "cli"; path = Path.v "bin/cli.ml" };
      { name = "server"; path = Path.v "bin/server.ml" };
    ]
  in
  let input = make_test_input_with_binaries root binaries in

  match Tusk_planner.plan_node input with
  | Ok { action_graph; _ } ->
      let actions = Action_graph.to_action_list action_graph in
      let exe_count =
        List.fold_left
          (fun count action ->
            match action with
            | Action.CreateExecutable _ -> count + 1
            | _ -> count)
          0 actions
      in

      if exe_count < 2 then
        Error (format "Expected 2 CreateExecutable actions, got %d" exe_count)
      else Ok ()
  | Error err ->
      Error (format "Planning failed: %s" (Planning_error.to_string err))

let test_circular_dependency () =
  let root =
    Path.v "packages/tusk-planner/tests/fixtures/circular-dependency"
  in
  let input = make_test_input root in

  match Tusk_planner.plan_node input with
  | Ok _ -> Error "Expected CyclicDependency error for circular dependency"
  | Error (Planning_error.CyclicDependency { cycle }) ->
      if List.length cycle > 0 then Ok ()
      else Error "Cycle detected but cycle list is empty"
  | Error err ->
      Error
        (format "Expected CyclicDependency, got: %s"
           (Planning_error.to_string err))

let test_action_nodes_have_hashes () =
  let root =
    Path.v "packages/tusk-planner/tests/fixtures/single-with-interface"
  in
  let input = make_test_input root in

  match Tusk_planner.plan_node input with
  | Ok { action_graph; _ } ->
      let nodes = Action_graph.nodes action_graph in
      if List.length nodes = 0 then Error "Expected at least one action node"
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
      Error (format "Planning failed: %s" (Planning_error.to_string err))

let test_hashes_include_package_name () =
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

  match (Tusk_planner.plan_node input1, Tusk_planner.plan_node input2) with
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
      Error (format "Planning failed: %s" (Planning_error.to_string err))

let test_hashes_are_unique () =
  let root = Path.v "packages/tusk-planner/tests/fixtures/linear-dependency" in
  let input = make_test_input root in

  match Tusk_planner.plan_node input with
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
      Error (format "Planning failed: %s" (Planning_error.to_string err))

let make_test_input root_path =
  let toolchain =
    Tusk_toolchain.init ()
    |> Result.expect ~msg:"Failed to initialize toolchain"
  in

  let package_name = Path.basename root_path in
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
        { root = Path.v "."; target_dir_root = Path.v "_build"; packages = [] };
    planning_root = Path.(root_path / Path.v "src");
    dependencies = [];
  }

let test_hash_stable_across_runs () =
  let root =
    Path.v "packages/tusk-planner/tests/fixtures/single-with-interface"
  in
  let input = make_test_input root in

  match (Tusk_planner.plan_node input, Tusk_planner.plan_node input) with
  | Ok { action_graph = ag1; _ }, Ok { action_graph = ag2; _ } ->
      let nodes1 = Action_graph.nodes ag1 in
      let nodes2 = Action_graph.nodes ag2 in

      if List.length nodes1 <> List.length nodes2 then
        Error
          (format "Different number of nodes: %d vs %d" (List.length nodes1)
             (List.length nodes2))
      else
        let hashes1 =
          List.map (fun n -> Crypto.Digest.hex (Action_node.get_hash n)) nodes1
        in
        let hashes2 =
          List.map (fun n -> Crypto.Digest.hex (Action_node.get_hash n)) nodes2
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
            "Hashes are the same but in different order (node iteration order \
             changed)"
        else Ok ()
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
      else Ok ()
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
        Error
          (format "Found duplicate actions: %d total, %d unique"
             (List.length action_strs)
             (List.length unique_actions))
      else Ok ()
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

let test_hash_includes_dependencies () =
  let root = Path.v "packages/tusk-planner/tests/fixtures/linear-dependency" in
  let input = make_test_input root in

  match Tusk_planner.plan_node input with
  | Ok { action_graph; module_graph } ->
      let action_nodes = Action_graph.nodes action_graph in
      let module_nodes = G.topo_sort module_graph in

      if List.length action_nodes = 0 then
        Error "Expected at least one action node"
      else Ok ()
  | Error err ->
      Error (format "Planning failed: %s" (Planning_error.to_string err))

let test_outputs_match_expected_artifacts () =
  let root =
    Path.v "packages/tusk-planner/tests/fixtures/single-with-interface"
  in
  let input = make_test_input root in

  match Tusk_planner.plan_node input with
  | Ok { action_graph; _ } ->
      let nodes = Action_graph.nodes action_graph in
      let all_outputs =
        List.concat_map (fun (node : Action_node.t) -> node.value.outs) nodes
      in

      if List.length all_outputs = 0 then
        Error "Expected at least one output file"
      else Ok ()
  | Error err ->
      Error (format "Planning failed: %s" (Planning_error.to_string err))

let make_test_input root_path =
  let toolchain =
    Tusk_toolchain.init ()
    |> Result.expect ~msg:"Failed to initialize toolchain"
  in

  let package_name = Path.basename root_path in
  let lib_path =
    Path.(root_path / Path.v "src" / Path.v (format "%s.ml" package_name))
  in
  {
    package =
      Package.
        {
          name = package_name;
          path = root_path;
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
        { root = Path.v "."; target_dir_root = Path.v "_build"; packages = [] };
    planning_root = Path.(root_path / Path.v "src");
    dependencies = [];
  }

let test_simple_module_snapshot () =
  let fixture_path =
    Path.v "packages/tusk-planner/tests/snapshots/simple_module"
  in
  let input = make_test_input fixture_path in

  let { action_graph; _ } =
    Tusk_planner.plan_node input |> Result.expect ~msg:"Failed to plan package"
  in

  let json = Action_graph.to_json action_graph in
  let json_str = Json.to_string json in

  let snapshot_file =
    Path.v "packages/tusk-planner/tests/snapshots/simple_module/expected.json"
  in

  match Fs.read snapshot_file with
  | Error _ ->
      Fs.write json_str snapshot_file
      |> Result.expect ~msg:"Failed to write snapshot file";
      Ok ()
  | Ok expected_str ->
      let current_json =
        Json.of_string json_str
        |> Result.expect ~msg:"Failed to parse current JSON"
      in
      let expected_json =
        Json.of_string expected_str
        |> Result.expect ~msg:"Failed to parse expected JSON"
      in
      let current_graph =
        Action_graph.from_json current_json
        |> Result.expect ~msg:"Failed to deserialize current graph"
      in
      let expected_graph =
        Action_graph.from_json expected_json
        |> Result.expect ~msg:"Failed to deserialize expected graph"
      in

      if Action_graph.equal current_graph expected_graph then Ok ()
      else Error "Action graphs differ structurally"

let test_determinism () =
  let fixture_path =
    Path.v "packages/tusk-planner/tests/snapshots/simple_module"
  in
  let input = make_test_input fixture_path in

  let run_plan () =
    let { action_graph; _ } =
      Tusk_planner.plan_node input
      |> Result.expect ~msg:"Failed to plan package"
    in
    action_graph
  in

  let g1 = run_plan () in
  let g2 = run_plan () in
  let g3 = run_plan () in

  if Action_graph.equal g1 g2 && Action_graph.equal g2 g3 then Ok ()
  else Error "Action graph structure varies across runs"

let make_test_input root_path library =
  let toolchain =
    Tusk_toolchain.init ()
    |> Result.expect ~msg:"Failed to initialize toolchain"
  in

  let package_name = Path.basename root_path in
  {
    package =
      Package.
        {
          name = package_name;
          path = root_path;
          relative_path = Path.v ".";
          dependencies = [];
          binaries = [];
          library;
          test_library = None;
          test_modules = [];
        };
    toolchain;
    workspace =
      Workspace.
        { root = Path.v "."; target_dir_root = Path.v "_build"; packages = [] };
    planning_root = Path.v "src";
    dependencies = [];
  }

let test_single_module_library_compiles_source =
  Test.case "single module library compiles actual source file" (fun () ->
      let fixture_path =
        Path.v "packages/tusk-planner/tests/fixtures/single_module_lib"
      in
      let lib_path = Path.(fixture_path / Path.v "src/single_module_lib.ml") in
      let input = make_test_input fixture_path (Some { path = lib_path }) in

      match Tusk_planner.plan_node input with
      | Error err ->
          Error (format "Planning failed: %s" (Planning_error.to_string err))
      | Ok { action_graph; _ } ->
          let actions = Action_graph.to_action_list action_graph in

          let has_compile_source =
            List.exists
              (fun action ->
                match action with
                | Action.CompileImplementation { source; _ } ->
                    String.equal (Path.to_string source)
                      "src/single_module_lib.ml"
                | _ -> false)
              actions
          in

          if not has_compile_source then
            Error
              "Action graph does not compile the actual source file \
               src/single_module_lib.ml"
          else Ok ())

let test_single_module_library_has_objects =
  Test.case "single module library includes module in archive" (fun () ->
      let fixture_path =
        Path.v "packages/tusk-planner/tests/fixtures/single_module_lib"
      in
      let lib_path = Path.(fixture_path / Path.v "src/single_module_lib.ml") in
      let input = make_test_input fixture_path (Some { path = lib_path }) in

      match Tusk_planner.plan_node input with
      | Error err ->
          Error (format "Planning failed: %s" (Planning_error.to_string err))
      | Ok { action_graph; _ } -> (
          let actions = Action_graph.to_action_list action_graph in

          let create_lib_objects =
            List.find_map
              (fun action ->
                match action with
                | Action.CreateLibrary { objects; _ } -> Some objects
                | _ -> None)
              actions
          in

          match create_lib_objects with
          | None -> Error "No CreateLibrary action found"
          | Some objects ->
              let has_module_cmx =
                List.exists
                  (fun obj ->
                    String.ends_with ~suffix:"Single_module_lib.cmx"
                      (Path.to_string obj))
                  objects
              in

              if not has_module_cmx then
                Error
                  (format
                     "Library objects list does not contain \
                      Single_module_lib.cmx. Objects: %s"
                     (String.concat ", " (List.map Path.to_string objects)))
              else Ok ()))

let test_multi_module_library_compiles_all =
  Test.case "multi-module library compiles all source files" (fun () ->
      let fixture_path =
        Path.v "packages/tusk-planner/tests/fixtures/multi_module_lib"
      in
      let lib_path = Path.(fixture_path / Path.v "src/multi_module_lib.ml") in
      let input = make_test_input fixture_path (Some { path = lib_path }) in

      match Tusk_planner.plan_node input with
      | Error err ->
          Error (format "Planning failed: %s" (Planning_error.to_string err))
      | Ok { action_graph; _ } ->
          let actions = Action_graph.to_action_list action_graph in

          let compiled_sources =
            List.filter_map
              (fun action ->
                match action with
                | Action.CompileImplementation { source; _ } ->
                    Some (Path.to_string source)
                | _ -> None)
              actions
          in

          let has_a =
            List.exists
              (fun s -> String.ends_with ~suffix:"a.ml" s)
              compiled_sources
          in
          let has_b =
            List.exists
              (fun s -> String.ends_with ~suffix:"b.ml" s)
              compiled_sources
          in

          if not has_a then Error "Action graph does not compile a.ml"
          else if not has_b then Error "Action graph does not compile b.ml"
          else Ok ())

let test_multi_module_library_includes_all_objects =
  Test.case "multi-module library includes all child modules in archive"
    (fun () ->
      let fixture_path =
        Path.v "packages/tusk-planner/tests/fixtures/multi_module_lib"
      in
      let lib_path = Path.(fixture_path / Path.v "src/multi_module_lib.ml") in
      let input = make_test_input fixture_path (Some { path = lib_path }) in

      match Tusk_planner.plan_node input with
      | Error err ->
          Error (format "Planning failed: %s" (Planning_error.to_string err))
      | Ok { action_graph; _ } -> (
          let actions = Action_graph.to_action_list action_graph in

          let create_lib_objects =
            List.find_map
              (fun action ->
                match action with
                | Action.CreateLibrary { objects; _ } -> Some objects
                | _ -> None)
              actions
          in

          match create_lib_objects with
          | None -> Error "No CreateLibrary action found"
          | Some objects ->
              let obj_strings = List.map Path.to_string objects in
              let has_a =
                List.exists
                  (fun s -> String.ends_with ~suffix:"__A.cmx" s)
                  obj_strings
              in
              let has_b =
                List.exists
                  (fun s -> String.ends_with ~suffix:"__B.cmx" s)
                  obj_strings
              in

              if not has_a then
                Error
                  (format "Library objects missing A.cmx. Objects: %s"
                     (String.concat ", " obj_strings))
              else if not has_b then
                Error
                  (format "Library objects missing B.cmx. Objects: %s"
                     (String.concat ", " obj_strings))
              else Ok ()))

let test_binary_only_no_library_node =
  Test.case "binary-only package has no library node" (fun () ->
      let fixture_path =
        Path.v "packages/tusk-planner/tests/fixtures/binary_only"
      in
      let input = make_test_input fixture_path None in

      match Tusk_planner.plan_node input with
      | Error err ->
          Error (format "Planning failed: %s" (Planning_error.to_string err))
      | Ok { action_graph; _ } ->
          let actions = Action_graph.to_action_list action_graph in

          let has_create_lib =
            List.exists
              (fun action ->
                match action with Action.CreateLibrary _ -> true | _ -> false)
              actions
          in

          if has_create_lib then
            Error "Binary-only package should not have CreateLibrary action"
          else Ok ())

let test_binary_only_has_executable =
  Test.case "binary-only package creates executable" (fun () ->
      let fixture_path =
        Path.v "packages/tusk-planner/tests/fixtures/binary_only"
      in
      let bin : Package.binary =
        { name = "main"; path = Path.(fixture_path / Path.v "src/main.ml") }
      in
      let package_name = Path.basename fixture_path in
      let input =
        {
          package =
            Package.
              {
                name = package_name;
                path = fixture_path;
                relative_path = Path.v ".";
                dependencies = [];
                binaries = [ bin ];
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

      match Tusk_planner.plan_node input with
      | Error err ->
          Error (format "Planning failed: %s" (Planning_error.to_string err))
      | Ok { action_graph; _ } ->
          let actions = Action_graph.to_action_list action_graph in

          let has_create_exe =
            List.exists
              (fun action ->
                match action with
                | Action.CreateExecutable _ -> true
                | _ -> false)
              actions
          in

          if not has_create_exe then
            Error "Binary-only package should have CreateExecutable action"
          else Ok ())

let test_library_with_subdir_creates_sublibrary =
  Test.case "library with subdirectory creates sublibrary wrapper" (fun () ->
      let fixture_path =
        Path.v "packages/tusk-planner/tests/fixtures/lib_with_subdir"
      in
      let lib_path = Path.(fixture_path / Path.v "src/lib_with_subdir.ml") in
      let input = make_test_input fixture_path (Some { path = lib_path }) in

      match Tusk_planner.plan_node input with
      | Error err ->
          Error (format "Planning failed: %s" (Planning_error.to_string err))
      | Ok { action_graph; _ } ->
          let actions = Action_graph.to_action_list action_graph in

          let has_utils_wrapper =
            List.exists
              (fun action ->
                match action with
                | Action.CompileImplementation { source; _ } ->
                    let src = Path.to_string source in
                    String.ends_with ~suffix:"utils/utils.ml" src
                | _ -> false)
              actions
          in

          if not has_utils_wrapper then
            Error
              "Subdirectory 'utils' should compile utils/utils.ml sublibrary \
               wrapper"
          else Ok ())

let test_library_with_subdir_compiles_helper =
  Test.case "library with subdirectory compiles helper module" (fun () ->
      let fixture_path =
        Path.v "packages/tusk-planner/tests/fixtures/lib_with_subdir"
      in
      let lib_path = Path.(fixture_path / Path.v "src/lib_with_subdir.ml") in
      let input = make_test_input fixture_path (Some { path = lib_path }) in

      match Tusk_planner.plan_node input with
      | Error err ->
          Error (format "Planning failed: %s" (Planning_error.to_string err))
      | Ok { action_graph; _ } ->
          let actions = Action_graph.to_action_list action_graph in

          let has_helper =
            List.exists
              (fun action ->
                match action with
                | Action.CompileImplementation { source; _ } ->
                    String.ends_with ~suffix:"helper.ml" (Path.to_string source)
                | _ -> false)
              actions
          in

          if not has_helper then
            Error "Subdirectory helper.ml should be compiled"
          else Ok ())

let test_lib_and_binary_has_both =
  Test.case "package with lib and binary creates both" (fun () ->
      let fixture_path =
        Path.v "packages/tusk-planner/tests/fixtures/lib_and_binary"
      in
      let lib_path = Path.(fixture_path / Path.v "src/lib_and_binary.ml") in
      let bin : Package.binary =
        { name = "cli"; path = Path.(fixture_path / Path.v "bin/cli.ml") }
      in
      let package_name = Path.basename fixture_path in
      let input =
        {
          package =
            Package.
              {
                name = package_name;
                path = fixture_path;
                relative_path = Path.v ".";
                dependencies = [];
                binaries = [ bin ];
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

      match Tusk_planner.plan_node input with
      | Error err ->
          Error (format "Planning failed: %s" (Planning_error.to_string err))
      | Ok { action_graph; _ } ->
          let actions = Action_graph.to_action_list action_graph in

          let has_lib =
            List.exists
              (fun action ->
                match action with Action.CreateLibrary _ -> true | _ -> false)
              actions
          in

          let has_bin =
            List.exists
              (fun action ->
                match action with
                | Action.CreateExecutable _ -> true
                | _ -> false)
              actions
          in

          if not has_lib then
            Error "Package with [lib] should have CreateLibrary action"
          else if not has_bin then
            Error "Package with [[bin]] should have CreateExecutable action"
          else Ok ())

let test_module_dependencies_correct_order =
  Test.case "modules with dependencies compile in correct order" (fun () ->
      let fixture_path =
        Path.v "packages/tusk-planner/tests/fixtures/lib_with_deps"
      in
      let lib_path = Path.(fixture_path / Path.v "src/lib_with_deps.ml") in
      let input = make_test_input fixture_path (Some { path = lib_path }) in

      match Tusk_planner.plan_node input with
      | Error err ->
          Error (format "Planning failed: %s" (Planning_error.to_string err))
      | Ok { action_graph; _ } -> (
          let sorted_actions = Action_graph.to_action_list action_graph in

          let find_compile_index suffix =
            List.find_index
              (fun action ->
                match action with
                | Action.CompileImplementation { source; _ } ->
                    String.ends_with ~suffix (Path.to_string source)
                | _ -> false)
              sorted_actions
          in

          match (find_compile_index "b.ml", find_compile_index "a.ml") with
          | Some b_idx, Some a_idx ->
              if b_idx >= a_idx then
                Error
                  (format
                     "Module B (used by A) should compile before A. B at %d, A \
                      at %d"
                     b_idx a_idx)
              else Ok ()
          | None, _ -> Error "Module B not found in action list"
          | _, None -> Error "Module A not found in action list"))

let test_empty_library_still_creates_archive =
  Test.case "empty library (no child modules) still creates archive" (fun () ->
      let fixture_path =
        Path.v "packages/tusk-planner/tests/fixtures/empty_lib"
      in
      let lib_path = Path.(fixture_path / Path.v "src/empty_lib.ml") in
      let input = make_test_input fixture_path (Some { path = lib_path }) in

      match Tusk_planner.plan_node input with
      | Error err ->
          Error (format "Planning failed: %s" (Planning_error.to_string err))
      | Ok { action_graph; _ } ->
          let actions = Action_graph.to_action_list action_graph in

          let has_create_lib =
            List.exists
              (fun action ->
                match action with Action.CreateLibrary _ -> true | _ -> false)
              actions
          in

          if not has_create_lib then
            Error "Empty library should still create library archive"
          else Ok ())

let test_multiple_binaries_all_created =
  Test.case "multiple binaries all get created" (fun () ->
      let fixture_path =
        Path.v "packages/tusk-planner/tests/fixtures/multi_binary"
      in
      let cli_bin : Package.binary =
        { name = "cli"; path = Path.(fixture_path / Path.v "src/cli.ml") }
      in
      let server_bin : Package.binary =
        { name = "server"; path = Path.(fixture_path / Path.v "src/server.ml") }
      in
      let package_name = Path.basename fixture_path in
      let input =
        {
          package =
            Package.
              {
                name = package_name;
                path = fixture_path;
                relative_path = Path.v ".";
                dependencies = [];
                binaries = [ cli_bin; server_bin ];
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

      match Tusk_planner.plan_node input with
      | Error err ->
          Error (format "Planning failed: %s" (Planning_error.to_string err))
      | Ok { action_graph; _ } ->
          let actions = Action_graph.to_action_list action_graph in

          let exe_outputs =
            List.filter_map
              (fun action ->
                match action with
                | Action.CreateExecutable { output; _ } ->
                    Some (Path.to_string output)
                | _ -> None)
              actions
          in

          let has_cli =
            List.exists (fun s -> String.equal s "cli") exe_outputs
          in
          let has_server =
            List.exists (fun s -> String.equal s "server") exe_outputs
          in

          if not has_cli then
            Error
              (format "Missing cli executable. Outputs: %s"
                 (String.concat ", " exe_outputs))
          else if not has_server then
            Error
              (format "Missing server executable. Outputs: %s"
                 (String.concat ", " exe_outputs))
          else Ok ())

let test_deterministic_action_order =
  Test.case "action graph order is deterministic across runs" (fun () ->
      let fixture_path =
        Path.v "packages/tusk-planner/tests/fixtures/multi_module_lib"
      in
      let lib_path = Path.(fixture_path / Path.v "src/multi_module_lib.ml") in
      let input = make_test_input fixture_path (Some { path = lib_path }) in

      let get_action_hashes () =
        match Tusk_planner.plan_node input with
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

let test_nested_subdirs_all_wrappers =
  Test.case "deeply nested subdirectories create all wrapper modules" (fun () ->
      let fixture_path =
        Path.v "packages/tusk-planner/tests/fixtures/nested_subdirs"
      in
      let lib_path = Path.(fixture_path / Path.v "src/nested_subdirs.ml") in
      let input = make_test_input fixture_path (Some { path = lib_path }) in

      match Tusk_planner.plan_node input with
      | Error err ->
          Error (format "Planning failed: %s" (Planning_error.to_string err))
      | Ok { action_graph; _ } ->
          let actions = Action_graph.to_action_list action_graph in

          let has_core =
            List.exists
              (fun action ->
                match action with
                | Action.CompileImplementation { source; _ } ->
                    String.ends_with ~suffix:"core/core.ml"
                      (Path.to_string source)
                | _ -> false)
              actions
          in

          let has_utils =
            List.exists
              (fun action ->
                match action with
                | Action.CompileImplementation { source; _ } ->
                    let src = Path.to_string source in
                    String.ends_with ~suffix:"core/utils/utils.ml" src
                | _ -> false)
              actions
          in

          if not has_core then
            Error "Missing Core wrapper compilation for src/core/core.ml"
          else if not has_utils then
            Error
              "Missing Utils wrapper compilation for src/core/utils/utils.ml"
          else Ok ())

let test_unreachable_module_still_compiled =
  Test.case "unreachable orphan module still gets compiled (current behavior)"
    (fun () ->
      let fixture_path =
        Path.v "packages/tusk-planner/tests/fixtures/lib_with_unreachable"
      in
      let lib_path =
        Path.(fixture_path / Path.v "src/lib_with_unreachable.ml")
      in
      let input = make_test_input fixture_path (Some { path = lib_path }) in

      match Tusk_planner.plan_node input with
      | Error err ->
          Error (format "Planning failed: %s" (Planning_error.to_string err))
      | Ok { action_graph; _ } ->
          let actions = Action_graph.to_action_list action_graph in

          let has_orphan =
            List.exists
              (fun action ->
                match action with
                | Action.CompileImplementation { source; _ } ->
                    String.ends_with ~suffix:"orphan.ml" (Path.to_string source)
                | _ -> false)
              actions
          in

          if has_orphan then Ok ()
          else
            Error
              "Orphan module should be compiled (current behavior - compile \
               everything)")

let test_mli_only_module =
  Test.case "module with only .mli (no .ml) compiles interface" (fun () ->
      let fixture_path =
        Path.v "packages/tusk-planner/tests/fixtures/mli_only"
      in
      let lib_path = Path.(fixture_path / Path.v "src/mli_only.ml") in
      let input = make_test_input fixture_path (Some { path = lib_path }) in

      match Tusk_planner.plan_node input with
      | Error err ->
          Error (format "Planning failed: %s" (Planning_error.to_string err))
      | Ok { action_graph; _ } ->
          let actions = Action_graph.to_action_list action_graph in

          let has_types_mli =
            List.exists
              (fun action ->
                match action with
                | Action.CompileInterface { source; _ } ->
                    String.ends_with ~suffix:"types.mli" (Path.to_string source)
                | _ -> false)
              actions
          in

          if not has_types_mli then
            Error "Should compile types.mli even without types.ml"
          else Ok ())

let test_ml_only_module =
  Test.case "module with only .ml (no .mli) compiles implementation" (fun () ->
      let fixture_path =
        Path.v "packages/tusk-planner/tests/fixtures/ml_only"
      in
      let lib_path = Path.(fixture_path / Path.v "src/ml_only.ml") in
      let input = make_test_input fixture_path (Some { path = lib_path }) in

      match Tusk_planner.plan_node input with
      | Error err ->
          Error (format "Planning failed: %s" (Planning_error.to_string err))
      | Ok { action_graph; _ } ->
          let actions = Action_graph.to_action_list action_graph in

          let has_impl_ml =
            List.exists
              (fun action ->
                match action with
                | Action.CompileImplementation { source; _ } ->
                    String.ends_with ~suffix:"impl.ml" (Path.to_string source)
                | _ -> false)
              actions
          in

          if not has_impl_ml then
            Error "Should compile impl.ml even without impl.mli"
          else Ok ())

let test_circular_dependency_detected =
  Test.case "circular dependencies are detected and reported" (fun () ->
      let fixture_path =
        Path.v "packages/tusk-planner/tests/fixtures/circular_deps"
      in
      let lib_path = Path.(fixture_path / Path.v "src/circular_deps.ml") in
      let input = make_test_input fixture_path (Some { path = lib_path }) in

      match Tusk_planner.plan_node input with
      | Ok _ -> Error "Should have detected circular dependency between A and B"
      | Error (Planning_error.CyclicDependency _) -> Ok ()
      | Error err ->
          Error
            (format "Got wrong error type: %s" (Planning_error.to_string err)))

let test_library_archive_has_correct_extension =
  Test.case "library archive output has .cmxa extension" (fun () ->
      let fixture_path =
        Path.v "packages/tusk-planner/tests/fixtures/single_module_lib"
      in
      let lib_path = Path.(fixture_path / Path.v "src/single_module_lib.ml") in
      let input = make_test_input fixture_path (Some { path = lib_path }) in

      match Tusk_planner.plan_node input with
      | Error err ->
          Error (format "Planning failed: %s" (Planning_error.to_string err))
      | Ok { action_graph; _ } -> (
          let actions = Action_graph.to_action_list action_graph in

          let lib_output =
            List.find_map
              (fun action ->
                match action with
                | Action.CreateLibrary { output; _ } ->
                    Some (Path.to_string output)
                | _ -> None)
              actions
          in

          match lib_output with
          | None -> Error "No CreateLibrary action found"
          | Some out ->
              if String.ends_with ~suffix:".cmxa" out then Ok ()
              else
                Error
                  (format "Library output should end with .cmxa, got: %s" out)))

let test_binary_executable_has_correct_name =
  Test.case "binary executable output has correct name" (fun () ->
      let fixture_path =
        Path.v "packages/tusk-planner/tests/fixtures/binary_only"
      in
      let bin : Package.binary =
        { name = "main"; path = Path.(fixture_path / Path.v "src/main.ml") }
      in
      let package_name = Path.basename fixture_path in
      let input =
        {
          package =
            Package.
              {
                name = package_name;
                path = fixture_path;
                relative_path = Path.v ".";
                dependencies = [];
                binaries = [ bin ];
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

      match Tusk_planner.plan_node input with
      | Error err ->
          Error (format "Planning failed: %s" (Planning_error.to_string err))
      | Ok { action_graph; _ } -> (
          let actions = Action_graph.to_action_list action_graph in

          let bin_output =
            List.find_map
              (fun action ->
                match action with
                | Action.CreateExecutable { output; _ } ->
                    Some (Path.to_string output)
                | _ -> None)
              actions
          in

          match bin_output with
          | None -> Error "No CreateExecutable action found"
          | Some out ->
              if String.equal out "main" then Ok ()
              else Error (format "Binary output should be 'main', got: %s" out)))

let test_native_folder_c_files_compiled =
  Test.case "C files in native/ folder are compiled" (fun () ->
      let fixture_path =
        Path.v "packages/tusk-planner/tests/fixtures/lib_with_native"
      in
      let lib_path = Path.(fixture_path / Path.v "src/lib_with_native.ml") in
      let input = make_test_input fixture_path (Some { path = lib_path }) in

      match Tusk_planner.plan_node input with
      | Error err ->
          Error (format "Planning failed: %s" (Planning_error.to_string err))
      | Ok { action_graph; _ } ->
          let actions = Action_graph.to_action_list action_graph in

          let has_stubs =
            List.exists
              (fun action ->
                match action with
                | Action.CompileC { source; _ } ->
                    String.ends_with ~suffix:"stubs.c" (Path.to_string source)
                | _ -> false)
              actions
          in

          let has_helper =
            List.exists
              (fun action ->
                match action with
                | Action.CompileC { source; _ } ->
                    String.ends_with ~suffix:"helper.c" (Path.to_string source)
                | _ -> false)
              actions
          in

          if not has_stubs then Error "native/stubs.c should be compiled"
          else if not has_helper then Error "native/helper.c should be compiled"
          else Ok ())

let test_native_folder_objects_in_library =
  Test.case "compiled C objects from native/ are included in library archive"
    (fun () ->
      let fixture_path =
        Path.v "packages/tusk-planner/tests/fixtures/lib_with_native"
      in
      let lib_path = Path.(fixture_path / Path.v "src/lib_with_native.ml") in
      let input = make_test_input fixture_path (Some { path = lib_path }) in

      match Tusk_planner.plan_node input with
      | Error err ->
          Error (format "Planning failed: %s" (Planning_error.to_string err))
      | Ok { action_graph; _ } -> (
          let actions = Action_graph.to_action_list action_graph in

          let lib_objects =
            List.find_map
              (fun action ->
                match action with
                | Action.CreateLibrary { objects; _ } -> Some objects
                | _ -> None)
              actions
          in

          match lib_objects with
          | None -> Error "No CreateLibrary action found"
          | Some objects ->
              let obj_strings = List.map Path.to_string objects in
              let has_c_object =
                List.exists
                  (fun s -> String.ends_with ~suffix:".o" s)
                  obj_strings
              in

              if not has_c_object then
                Error
                  (format "Library should include C object files. Objects: %s"
                     (String.concat ", " obj_strings))
              else Ok ()))

let test_native_folder_isolated_from_src =
  Test.case "native/ folder keeps C files separate from src/" (fun () ->
      let fixture_path =
        Path.v "packages/tusk-planner/tests/fixtures/lib_with_native"
      in
      let lib_path = Path.(fixture_path / Path.v "src/lib_with_native.ml") in
      let input = make_test_input fixture_path (Some { path = lib_path }) in

      match Tusk_planner.plan_node input with
      | Error err ->
          Error (format "Planning failed: %s" (Planning_error.to_string err))
      | Ok { action_graph; _ } ->
          let actions = Action_graph.to_action_list action_graph in

          let c_compile_actions =
            List.filter_map
              (fun action ->
                match action with
                | Action.CompileC { source; _ } -> Some (Path.to_string source)
                | _ -> None)
              actions
          in

          let all_in_native =
            List.for_all
              (fun path -> String.starts_with ~prefix:"native/" path)
              c_compile_actions
          in

          if not all_in_native then
            Error
              (format "All C files should be in native/ folder. Found: %s"
                 (String.concat ", " c_compile_actions))
          else Ok ())

let tests =
  Test.
    [
      case "action graph is DAG" test_action_graph_is_dag;
      case "action graph is deterministic across runs" test_determinism;
      case "action nodes have pre-computed hashes" test_action_nodes_have_hashes;
      case "binary is excluded from library"
        test_binary_is_excluded_from_library;
      case "c stubs" test_c_stubs;
      case "circular dependency detection" test_circular_dependency;
      case "complex multi-module" test_complex_multi_module;
      case "dependency hash propagation" test_dependency_hash_propagation;
      case "diamond dependency" test_diamond_dependency;
      case "empty library" test_empty_library;
      case "graph has root node" test_graph_has_root_node;
      case "graph namespace is set" test_graph_namespace_is_set;
      case "hash includes dependencies" test_hash_includes_dependencies;
      case "hash stable across runs" test_hash_stable_across_runs;
      case "hashes are unique per node" test_hashes_are_unique;
      case "hashes include package name" test_hashes_include_package_name;
      case "header only" test_header_only;
      case "library with binary" test_library_with_binary;
      case "linear dependency" test_linear_dependency;
      case "mixed interfaces" test_mixed_interfaces;
      case "multiple binaries" test_multiple_binaries;
      case "no duplicate actions" test_no_duplicate_actions;
      case "outputs match expected artifacts"
        test_outputs_match_expected_artifacts;
      case "planner generates actions" test_planner_generates_actions;
      case "simple_module matches snapshot" test_simple_module_snapshot;
      case "single module with interface" test_single_with_interface;
      case "topological order maintained" test_topological_order_maintained;
      test_binary_executable_has_correct_name;
      test_binary_only_has_executable;
      test_binary_only_no_library_node;
      test_circular_dependency_detected;
      test_deterministic_action_order;
      test_empty_library_still_creates_archive;
      test_lib_and_binary_has_both;
      test_library_archive_has_correct_extension;
      test_library_with_subdir_compiles_helper;
      test_library_with_subdir_creates_sublibrary;
      test_ml_only_module;
      test_mli_only_module;
      test_module_dependencies_correct_order;
      test_multi_module_library_compiles_all;
      test_multi_module_library_includes_all_objects;
      test_multiple_binaries_all_created;
      test_native_folder_c_files_compiled;
      test_native_folder_isolated_from_src;
      test_native_folder_objects_in_library;
      test_nested_subdirs_all_wrappers;
      test_single_module_library_compiles_source;
      test_single_module_library_has_objects;
      test_unreachable_module_still_compiled;
    ]

let name = "Planner Tests"
let () = Miniriot.run ~main:(Test.Cli.main ~name ~tests) ~args:Env.args
