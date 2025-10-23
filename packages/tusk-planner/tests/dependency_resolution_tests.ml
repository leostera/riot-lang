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

let test_linear_dependency =
  Test.case "linear dependency (A depends on B)" (fun () ->
      let root =
        Path.v "packages/tusk-planner/tests/fixtures/linear-dependency"
      in
      let input = make_test_input root in

      match Tusk_planner.Module_planner.plan_node input with
      | Ok { module_graph; action_graph } ->
          let actions = Action_graph.to_action_list action_graph in
          let nodes = G.topo_sort module_graph in

          if List.length nodes < 3 then
            Error
              (format "Expected at least 3 nodes (Root + A + B), got %d"
                 (List.length nodes))
          else if List.length actions < 3 then
            Error
              (format
                 "Expected at least 3 actions (2 compiles + library), got %d"
                 (List.length actions))
          else Ok ()
      | Error err ->
          Error (format "Planning failed: %s" (Planning_error.to_string err)))

let test_diamond_dependency =
  Test.case "diamond dependency (A→B,C→D)" (fun () ->
      let root =
        Path.v "packages/tusk-planner/tests/fixtures/diamond-dependency"
      in
      let input = make_test_input root in

      match Tusk_planner.Module_planner.plan_node input with
      | Ok { module_graph; _ } ->
          let nodes = G.topo_sort module_graph in
          if List.length nodes = 0 then
            Error "Expected at least one module node"
          else Ok ()
      | Error err ->
          Error (format "Planning failed: %s" (Planning_error.to_string err)))

let test_circular_dependency_detected =
  Test.case "circular dependencies are detected and reported" (fun () ->
      let fixture_path =
        Path.v "packages/tusk-planner/tests/fixtures/circular_deps"
      in
      let lib_path = Path.(fixture_path / Path.v "src/circular_deps.ml") in
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

      match Tusk_planner.Module_planner.plan_node input with
      | Ok _ -> Error "Expected CyclicDependency error for circular dependency"
      | Error (Planning_error.CyclicDependency { cycle }) ->
          if List.length cycle > 0 then Ok ()
          else Error "Cycle detected but cycle list is empty"
      | Error err ->
          Error
            (format "Expected CyclicDependency, got: %s"
               (Planning_error.to_string err)))

let test_module_dependencies_correct_order =
  Test.case "modules with dependencies compile in correct order" (fun () ->
      let fixture_path =
        Path.v "packages/tusk-planner/tests/fixtures/lib_with_deps"
      in
      let lib_path = Path.(fixture_path / Path.v "src/lib_with_deps.ml") in
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

      match Tusk_planner.Module_planner.plan_node input with
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

let test_complex_multi_module =
  Test.case "complex multi-module package with multiple dependencies" (fun () ->
      let root =
        Path.v "packages/tusk-planner/tests/fixtures/complex-multi-module"
      in
      let input = make_test_input root in

      match Tusk_planner.Module_planner.plan_node input with
      | Ok { module_graph; action_graph } ->
          let nodes = G.topo_sort module_graph in
          let actions = Action_graph.to_action_list action_graph in

          if List.length nodes < 8 then
            Error
              (format "Expected at least 8 nodes for complex module, got %d"
                 (List.length nodes))
          else if List.length actions < 6 then
            Error
              (format "Expected at least 6 actions, got %d"
                 (List.length actions))
          else Ok ()
      | Error err ->
          Error (format "Planning failed: %s" (Planning_error.to_string err)))

let tests =
  Test.
    [
      test_linear_dependency;
      test_diamond_dependency;
      test_circular_dependency_detected;
      test_module_dependencies_correct_order;
      test_complex_multi_module;
    ]

let name = "Dependency Resolution Tests"
let () = Miniriot.run ~main:(Test.Cli.main ~name ~tests) ~args:Env.args
