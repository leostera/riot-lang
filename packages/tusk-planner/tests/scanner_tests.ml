open Std
open Tusk_planner
open Tusk_model

let make_test_config root_path source_dir =
  Graph_builder.{
    root = root_path;
    source_dir;
    namespace = "Test";
    package = Package.{
      name = "test";
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
  }

let test_scan_simple_fixture () =
  let config = make_test_config (Path.v "tests/fixtures/simple") (Path.v "src") in
  let graph = Graph_builder.create config in
  if List.length graph.entries > 0 then Ok ()
  else Error "No entries scanned"

let test_scan_sublibrary_fixture () =
  let fixture_root = Path.v "tests/fixtures/sublibrary" in
  let config = make_test_config fixture_root fixture_root in
  let graph = Graph_builder.create config in
  if List.length graph.entries > 0 then Ok ()
  else Error "No entries scanned"

let tests = [
  Test.case "scan simple fixture" test_scan_simple_fixture;
  Test.case "scan sublibrary fixture" test_scan_sublibrary_fixture;
]

let () =
  Miniriot.run
    ~main:(fun ~args -> Test.Cli.main ~name:"Scanner Tests" ~tests ~args ())
    ~args:Env.args
  |> exit
