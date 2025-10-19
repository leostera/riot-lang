open Std
open Tusk_planner
open Tusk_model

module G = Std.Graph.SimpleGraph

let make_test_input root_path =
  let package_name = Path.basename root_path in
  Tusk_planner.{
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

let test_single_with_interface () =
  let root = Path.v "tests/fixtures/single-with-interface" in
  let input = make_test_input root in
  
  match Tusk_planner.plan_node input with
  | Ok { module_graph; action_graph } ->
      let actions = Action_graph.to_action_list action_graph in
      let module_count = List.length (G.topo_sort module_graph) in
      
      if module_count < 2 then
        Error (format "Expected at least 2 module nodes (MLI + ML), got %d" module_count)
      else if List.length actions < 2 then
        Error (format "Expected at least 2 actions (CompileInterface + CompileImplementation), got %d" 
          (List.length actions))
      else
        Ok ()
  | Error err ->
      Error (format "Planning failed: %s" (Planning_error.to_string err))

let test_linear_dependency () =
  let root = Path.v "tests/fixtures/linear-dependency" in
  let input = make_test_input root in
  
  match Tusk_planner.plan_node input with
  | Ok { module_graph; action_graph } ->
      let actions = Action_graph.to_action_list action_graph in
      let nodes = G.topo_sort module_graph in
      
      if List.length nodes < 3 then
        Error (format "Expected at least 3 nodes (Root + A + B), got %d" (List.length nodes))
      else if List.length actions < 3 then
        Error (format "Expected at least 3 actions (2 compiles + library), got %d"
          (List.length actions))
      else
        Ok ()
  | Error err ->
      Error (format "Planning failed: %s" (Planning_error.to_string err))

let test_c_stubs () =
  let root = Path.v "tests/fixtures/c-stubs" in
  let input = make_test_input root in
  
  match Tusk_planner.plan_node input with
  | Ok { module_graph; action_graph } ->
      let actions = Action_graph.to_action_list action_graph in
      let has_compile_c = List.exists (function
        | Action.CompileC _ -> true
        | _ -> false) actions in
      
      if not has_compile_c then
        Error "Expected CompileC action for .c file"
      else
        Ok ()
  | Error err ->
      Error (format "Planning failed: %s" (Planning_error.to_string err))

let test_diamond_dependency () =
  let root = Path.v "tests/fixtures/diamond-dependency" in
  let input = make_test_input root in
  
  match Tusk_planner.plan_node input with
  | Ok { module_graph; action_graph } ->
      let nodes = G.topo_sort module_graph in
      if List.length nodes < 4 then
        Error (format "Expected at least 4 module nodes (Base, Left, Right, Top), got %d" (List.length nodes))
      else
        Ok ()
  | Error err ->
      Error (format "Planning failed: %s" (Planning_error.to_string err))

let test_mixed_interfaces () =
  let root = Path.v "tests/fixtures/mixed-interfaces" in
  let input = make_test_input root in
  
  match Tusk_planner.plan_node input with
  | Ok { module_graph; action_graph } ->
      let nodes = G.topo_sort module_graph in
      if List.length nodes < 5 then
        Error (format "Expected at least 5 nodes (3 ML + 2 MLI), got %d" (List.length nodes))
      else
        Ok ()
  | Error err ->
      Error (format "Planning failed: %s" (Planning_error.to_string err))

let make_test_input_with_binaries root_path binaries =
  let package_name = Path.basename root_path in
  Tusk_planner.{
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

let test_library_with_binary () =
  let root = Path.v "tests/fixtures/library-with-binary" in
  let binaries = [Package.{ name = "main"; path = Path.v "bin/main.ml" }] in
  let input = make_test_input_with_binaries root binaries in
  
  match Tusk_planner.plan_node input with
  | Ok { module_graph; action_graph } ->
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
  let root = Path.v "tests/fixtures/multiple-binaries" in
  let binaries = [
    Package.{ name = "cli"; path = Path.v "bin/cli.ml" };
    Package.{ name = "server"; path = Path.v "bin/server.ml" };
  ] in
  let input = make_test_input_with_binaries root binaries in
  
  match Tusk_planner.plan_node input with
  | Ok { module_graph; action_graph } ->
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
  let root = Path.v "tests/fixtures/circular-dependency" in
  let input = make_test_input root in
  
  match Tusk_planner.plan_node input with
  | Ok _ ->
      Error "Expected CyclicDependency error for circular dependency"
  | Error (Planning_error.CyclicDependency { cycle }) ->
      if List.length cycle > 0 then Ok ()
      else Error "Cycle detected but cycle list is empty"
  | Error err ->
      Error (format "Expected CyclicDependency, got: %s" (Planning_error.to_string err))

let test_empty_library () =
  let root = Path.v "tests/fixtures/empty-library" in
  let input = make_test_input root in
  
  match Tusk_planner.plan_node input with
  | Ok { module_graph; action_graph } ->
      let actions = Action_graph.to_action_list action_graph in
      if List.length actions > 2 then
        Error (format "Expected minimal actions for empty library, got %d" (List.length actions))
      else
        Ok ()
  | Error err ->
      Error (format "Planning failed: %s" (Planning_error.to_string err))

let test_header_only () =
  let root = Path.v "tests/fixtures/header-only" in
  let input = make_test_input root in
  
  match Tusk_planner.plan_node input with
  | Ok { module_graph; action_graph } ->
      let actions = Action_graph.to_action_list action_graph in
      let has_compile = List.exists (function
        | Action.CompileC _ | Action.CompileImplementation _ | Action.CompileInterface _ -> true
        | _ -> false) actions in
      
      if has_compile then
        Error "Expected no compile actions for header-only files"
      else
        Ok ()
  | Error err ->
      Error (format "Planning failed: %s" (Planning_error.to_string err))

let test_complex_multi_module () =
  let root = Path.v "tests/fixtures/complex-multi-module" in
  let input = make_test_input root in
  
  match Tusk_planner.plan_node input with
  | Ok { module_graph; action_graph } ->
      let nodes = G.topo_sort module_graph in
      let actions = Action_graph.to_action_list action_graph in
      
      if List.length nodes < 8 then
        Error (format "Expected at least 8 nodes for complex module, got %d" (List.length nodes))
      else if List.length actions < 6 then
        Error (format "Expected at least 6 actions, got %d" (List.length actions))
      else
        Ok ()
  | Error err ->
      Error (format "Planning failed: %s" (Planning_error.to_string err))

let tests = [
  Test.case "single module with interface" test_single_with_interface;
  Test.case "linear dependency" test_linear_dependency;
  Test.case "c stubs" test_c_stubs;
  Test.case "diamond dependency" test_diamond_dependency;
  Test.case "mixed interfaces" test_mixed_interfaces;
  Test.case "library with binary" test_library_with_binary;
  Test.case "multiple binaries" test_multiple_binaries;
  Test.case "circular dependency" test_circular_dependency;
  Test.case "empty library" test_empty_library;
  Test.case "header only" test_header_only;
  Test.case "complex multi-module" test_complex_multi_module;
]

let () =
  Miniriot.run
    ~main:(fun ~args -> Test.Cli.main ~name:"Planner Tests" ~tests ~args ())
    ~args:Env.args
  |> exit
