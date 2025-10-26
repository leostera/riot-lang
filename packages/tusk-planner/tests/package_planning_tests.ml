open Std
open Std.Data
open Tusk_planner
open Tusk_model

let toolchain =
  Tusk_toolchain.init ~config:Tusk_model.Toolchain_config.default
  |> Result.expect ~msg:"Failed to initialize toolchain"

let make_test_input root_path library =
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
            library;
            sources = { src = []; native = []; tests = [] };
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

let test_single_module_library_compiles_source =
  Test.case "single module library compiles actual source file" (fun () ->
      let fixture_path =
        Path.v "packages/tusk-planner/tests/fixtures/single_module_lib"
      in
      let lib_path = Path.(fixture_path / Path.v "src/single_module_lib.ml") in
      let input = make_test_input fixture_path (Some { path = lib_path }) in

      match Tusk_planner.Module_planner.plan_node input with
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

      match Tusk_planner.Module_planner.plan_node input with
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

      match Tusk_planner.Module_planner.plan_node input with
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

      match Tusk_planner.Module_planner.plan_node input with
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

let test_empty_library_still_creates_archive =
  Test.case "empty library (no child modules) still creates archive" (fun () ->
      let fixture_path =
        Path.v "packages/tusk-planner/tests/fixtures/empty_lib"
      in
      let lib_path = Path.(fixture_path / Path.v "src/empty_lib.ml") in
      let input = make_test_input fixture_path (Some { path = lib_path }) in

      match Tusk_planner.Module_planner.plan_node input with
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

let test_library_archive_has_correct_extension =
  Test.case "library archive output has .cmxa extension" (fun () ->
      let fixture_path =
        Path.v "packages/tusk-planner/tests/fixtures/single_module_lib"
      in
      let lib_path = Path.(fixture_path / Path.v "src/single_module_lib.ml") in
      let input = make_test_input fixture_path (Some { path = lib_path }) in

      match Tusk_planner.Module_planner.plan_node input with
      | Error err ->
          Error (format "Planning failed: %s" (Planning_error.to_string err))
      | Ok { action_graph; _ } -> (
          let actions = Action_graph.to_action_list action_graph in

          let lib_output =
            List.find_map
              (fun action ->
                match action with
                | Action.CreateLibrary { outputs; _ } -> (
                    match outputs with
                    | output :: _ -> Some (Path.to_string output)
                    | [] -> None)
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

let test_binary_only_no_library_node =
  Test.case "binary-only package has no library node" (fun () ->
      let fixture_path =
        Path.v "packages/tusk-planner/tests/fixtures/binary_only"
      in
      let input = make_test_input fixture_path None in

      match Tusk_planner.Module_planner.plan_node input with
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
        Tusk_planner.Module_planner.
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
                  sources = { src = []; native = []; tests = [] };
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
        Tusk_planner.Module_planner.
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
                  sources = { src = []; native = []; tests = [] };
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
          let actions = Action_graph.to_action_list action_graph in

          let bin_output =
            List.find_map
              (fun action ->
                match action with
                | Action.CreateExecutable { outputs; _ } -> (
                    match outputs with
                    | output :: _ -> Some (Path.to_string output)
                    | [] -> None)
                | _ -> None)
              actions
          in

          match bin_output with
          | None -> Error "No CreateExecutable action found"
          | Some out ->
              if String.equal out "main" then Ok ()
              else Error (format "Binary output should be 'main', got: %s" out)))

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
        Tusk_planner.Module_planner.
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
                  sources = { src = []; native = []; tests = [] };
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
        Tusk_planner.Module_planner.
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
                  sources = { src = []; native = []; tests = [] };
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
      | Ok { action_graph; _ } ->
          let actions = Action_graph.to_action_list action_graph in

          let exe_outputs =
            List.filter_map
              (fun action ->
                match action with
                | Action.CreateExecutable { outputs; _ } -> (
                    match outputs with
                    | output :: _ -> Some (Path.to_string output)
                    | [] -> None)
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

let test_native_folder_objects_in_library =
  Test.case "compiled C objects from native/ are included in library archive"
    (fun () ->
      let fixture_path =
        Path.v "packages/tusk-planner/tests/fixtures/lib_with_native"
      in
      let lib_path = Path.(fixture_path / Path.v "src/lib_with_native.ml") in
      let input = make_test_input fixture_path (Some { path = lib_path }) in

      match Tusk_planner.Module_planner.plan_node input with
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

let tests =
  Test.
    [
      test_single_module_library_compiles_source;
      test_single_module_library_has_objects;
      test_multi_module_library_compiles_all;
      test_multi_module_library_includes_all_objects;
      test_empty_library_still_creates_archive;
      test_library_archive_has_correct_extension;
      test_binary_only_no_library_node;
      test_binary_only_has_executable;
      test_binary_executable_has_correct_name;
      test_lib_and_binary_has_both;
      test_multiple_binaries_all_created;
      test_native_folder_objects_in_library;
    ]

let name = "Package Planning Tests"
let () = Miniriot.run ~main:(Test.Cli.main ~name ~tests) ~args:Env.args
