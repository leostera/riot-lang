open Std
open Tusk_planner
open Tusk_model

let toolchain =
  Tusk_toolchain.init () |> Result.expect ~msg:"Failed to initialize toolchain"

let make_test_config root_path source_dir =
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

let test_scan_simple_fixture () =
  let config =
    make_test_config
      (Path.v "packages/tusk-planner/tests/fixtures/simple")
      (Path.v "src")
  in
  let graph = Graph_builder.create config in
  if List.length graph.entries > 0 then Ok () else Error "No entries scanned"

let test_scan_sublibrary_fixture () =
  let fixture_root = Path.v "packages/tusk-planner/tests/fixtures/sublibrary" in
  let config = make_test_config fixture_root (Path.v ".") in
  let graph = Graph_builder.create config in
  if List.length graph.entries > 0 then Ok () else Error "No entries scanned"

let tests =
  Test.
    [
      case "scan simple fixture" test_scan_simple_fixture;
      case "scan sublibrary fixture" test_scan_sublibrary_fixture;
    ]

let () =
  Miniriot.run ~main:(Test.Cli.main ~name:"Scanner Tests" ~tests) ~args:Env.args
