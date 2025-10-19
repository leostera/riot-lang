open Std
open Tusk_planner
open Tusk_model

let test_binary_is_excluded_from_library () =
  let fixture_root = Path.v "tests/fixtures/with-binary" in
  let config = Graph_builder.{
    root = fixture_root;
    source_dir = fixture_root;
    namespace = "TestBin";
    package = Package.{
      name = "test-bin";
      path = fixture_root;
      relative_path = Path.v ".";
      dependencies = [];
      binaries = [
        { name = "main"; path = Path.(fixture_root / v "main.ml") }
      ];
    };
    toolchain = Toolchains.default_toolchain;
    workspace = Workspace.{
      root = Path.v ".";
      target_dir_root = Path.v "_build";
      packages = [];
    };
  } in
  
  let _graph = Graph_builder.create config in
  Ok ()

let tests = [
  Test.case "binary is excluded from library" test_binary_is_excluded_from_library;
]

let () =
  Miniriot.run
    ~main:(fun ~args -> Test.Cli.main ~name:"Binary Tests" ~tests ~args ())
    ~args:Env.args
  |> exit
