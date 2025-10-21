open Std
open Std.Data
module G = Std.Graph.SimpleGraph
open Tusk_planner
open Tusk_model

let toolchain =
  Tusk_toolchain.init () |> Result.expect ~msg:"Failed to initialize toolchain"

let make_graph_config root_path source_dir =
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

let make_test_input root_path =
  let package_name = Path.basename root_path in
  Tusk_planner.Planner.
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

let test_scan_simple_fixture =
  Test.case "scan simple fixture" (fun () ->
      let config =
        make_graph_config
          (Path.v "packages/tusk-planner/tests/fixtures/simple")
          (Path.v "src")
      in
      let graph = Graph_builder.create config in
      if List.length graph.entries > 0 then Ok ()
      else Error "No entries scanned")

let test_scan_sublibrary_fixture =
  Test.case "scan sublibrary fixture" (fun () ->
      let fixture_root =
        Path.v "packages/tusk-planner/tests/fixtures/sublibrary"
      in
      let config = make_graph_config fixture_root (Path.v ".") in
      let graph = Graph_builder.create config in
      if List.length graph.entries > 0 then Ok ()
      else Error "No entries scanned")

let test_single_with_interface =
  Test.case "single module with interface (.ml + .mli)" (fun () ->
      let root =
        Path.v "packages/tusk-planner/tests/fixtures/single-with-interface"
      in
      let input = make_test_input root in

      match Tusk_planner.Planner.plan_node input with
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
          Error (format "Planning failed: %s" (Planning_error.to_string err)))

let test_mli_only_module =
  Test.case "module with only .mli (no .ml) compiles interface" (fun () ->
      let fixture_path =
        Path.v "packages/tusk-planner/tests/fixtures/mli_only"
      in
      let lib_path = Path.(fixture_path / Path.v "src/mli_only.ml") in
      let package_name = Path.basename fixture_path in
      let input =
        Tusk_planner.Planner.
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

      match Tusk_planner.Planner.plan_node input with
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
      let package_name = Path.basename fixture_path in
      let input =
        Tusk_planner.Planner.
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

      match Tusk_planner.Planner.plan_node input with
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

let test_mixed_interfaces =
  Test.case "mixed interfaces (some with .mli, some without)" (fun () ->
      let root =
        Path.v "packages/tusk-planner/tests/fixtures/mixed-interfaces"
      in
      let input = make_test_input root in

      match Tusk_planner.Planner.plan_node input with
      | Ok { module_graph; _ } ->
          let nodes = G.topo_sort module_graph in
          if List.length nodes = 0 then Error "Expected at least one node"
          else Ok ()
      | Error err ->
          Error (format "Planning failed: %s" (Planning_error.to_string err)))

let test_c_file_scanning =
  Test.case "C files are scanned and included in module graph" (fun () ->
      let root = Path.v "packages/tusk-planner/tests/fixtures/c-stubs" in
      let input = make_test_input root in

      match Tusk_planner.Planner.plan_node input with
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
            List.exists
              (function Action.CompileC _ -> true | _ -> false)
              actions
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
          Error (format "Planning failed: %s" (Planning_error.to_string err)))

let test_native_folder_c_files =
  Test.case "C files in native/ folder are scanned and compiled" (fun () ->
      let fixture_path =
        Path.v "packages/tusk-planner/tests/fixtures/lib_with_native"
      in
      let lib_path = Path.(fixture_path / Path.v "src/lib_with_native.ml") in
      let package_name = Path.basename fixture_path in
      let input =
        Tusk_planner.Planner.
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

      match Tusk_planner.Planner.plan_node input with
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

let test_native_folder_isolated_from_src =
  Test.case "native/ folder keeps C files separate from src/" (fun () ->
      let fixture_path =
        Path.v "packages/tusk-planner/tests/fixtures/lib_with_native"
      in
      let lib_path = Path.(fixture_path / Path.v "src/lib_with_native.ml") in
      let package_name = Path.basename fixture_path in
      let input =
        Tusk_planner.Planner.
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

      match Tusk_planner.Planner.plan_node input with
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

let test_subdirectory_creates_sublibrary =
  Test.case "subdirectory creates sublibrary wrapper module" (fun () ->
      let fixture_path =
        Path.v "packages/tusk-planner/tests/fixtures/lib_with_subdir"
      in
      let lib_path = Path.(fixture_path / Path.v "src/lib_with_subdir.ml") in
      let package_name = Path.basename fixture_path in
      let input =
        Tusk_planner.Planner.
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

      match Tusk_planner.Planner.plan_node input with
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

let test_subdirectory_compiles_helper =
  Test.case "subdirectory compiles helper modules" (fun () ->
      let fixture_path =
        Path.v "packages/tusk-planner/tests/fixtures/lib_with_subdir"
      in
      let lib_path = Path.(fixture_path / Path.v "src/lib_with_subdir.ml") in
      let package_name = Path.basename fixture_path in
      let input =
        Tusk_planner.Planner.
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

      match Tusk_planner.Planner.plan_node input with
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

let test_nested_subdirs_all_wrappers =
  Test.case "deeply nested subdirectories create all wrapper modules" (fun () ->
      let fixture_path =
        Path.v "packages/tusk-planner/tests/fixtures/nested_subdirs"
      in
      let lib_path = Path.(fixture_path / Path.v "src/nested_subdirs.ml") in
      let package_name = Path.basename fixture_path in
      let input =
        Tusk_planner.Planner.
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

      match Tusk_planner.Planner.plan_node input with
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
      let package_name = Path.basename fixture_path in
      let input =
        Tusk_planner.Planner.
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

      match Tusk_planner.Planner.plan_node input with
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

let tests =
  Test.
    [
      test_scan_simple_fixture;
      test_scan_sublibrary_fixture;
      test_single_with_interface;
      test_mli_only_module;
      test_ml_only_module;
      test_mixed_interfaces;
      test_c_file_scanning;
      test_native_folder_c_files;
      test_native_folder_isolated_from_src;
      test_subdirectory_creates_sublibrary;
      test_subdirectory_compiles_helper;
      test_nested_subdirs_all_wrappers;
      test_unreachable_module_still_compiled;
    ]

let name = "Module Scanner Tests"
let () = Miniriot.run ~main:(Test.Cli.main ~name ~tests) ~args:Env.args
