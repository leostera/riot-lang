open Std
module Test = Std.Test

let with_tempdir = fun prefix fn ->
  match Fs.with_tempdir ~prefix fn with
  | Ok result -> result
  | Error _ -> Error "Tempdir creation failed"

let parse_manifest = fun ~path ~relative_path toml ->
  Std.Data.Toml.parse toml
  |> Result.expect ~msg:"Expected package TOML to parse"
  |> Riot_model.Package_manifest.from_toml
    ~workspace_deps:[]
    ~workspace_dev_deps:[]
    ~workspace_build_deps:[]
    ~path
    ~relative_path
  |> Result.expect ~msg:"Expected package manifest to parse"

let binary_names = fun binaries ->
  binaries
  |> List.map ~fn:(fun (bin: Riot_model.Package.binary) -> bin.name)
  |> List.sort ~compare:String.compare

let test_manifest_from_toml_keeps_declared_metadata_only = fun _ctx ->
  with_tempdir "riot_model_package_manifest"
    (fun tmpdir ->
      let src_dir = Path.(tmpdir / Path.v "src") in
      let tests_dir = Path.(tmpdir / Path.v "tests") in
      Result.expect (Fs.create_dir_all src_dir) ~msg:"Failed to create src directory";
      Result.expect (Fs.create_dir_all tests_dir) ~msg:"Failed to create tests directory";
      Result.expect (Fs.write "let () = ()\n" Path.(src_dir / Path.v "main.ml")) ~msg:"Failed to write runtime source";
      Result.expect (Fs.write "let () = ()\n" Path.(tests_dir / Path.v "demo_tests.ml")) ~msg:"Failed to write test source";
      let manifest = parse_manifest
        ~path:tmpdir
        ~relative_path:(Path.v "packages/demo")
        {|
[package]
name = "demo"
version = "0.1.0"

[[bin]]
name = "custom"
path = "examples/custom.ml"
|}
      in
      if
        manifest.name = "demo"
        && manifest.declared_binaries = [ Riot_model.Package.{ name = "custom"; path = Path.v "examples/custom.ml" } ]
      then
        Ok ()
      else
        Error "expected package manifest to keep only declared binaries and metadata")

let test_manifest_realize_runtime_discovers_runtime_inputs_only = fun _ctx ->
  with_tempdir "riot_model_package_manifest_runtime"
    (fun tmpdir ->
      let src_dir = Path.(tmpdir / Path.v "src") in
      let tests_dir = Path.(tmpdir / Path.v "tests") in
      let examples_dir = Path.(tmpdir / Path.v "examples") in
      let bench_dir = Path.(tmpdir / Path.v "bench") in
      Result.expect (Fs.create_dir_all src_dir) ~msg:"Failed to create src directory";
      Result.expect (Fs.create_dir_all tests_dir) ~msg:"Failed to create tests directory";
      Result.expect (Fs.create_dir_all Path.(tests_dir / Path.v "fixtures")) ~msg:"Failed to create test fixtures directory";
      Result.expect (Fs.create_dir_all examples_dir) ~msg:"Failed to create examples directory";
      Result.expect (Fs.create_dir_all bench_dir) ~msg:"Failed to create bench directory";
      Result.expect (Fs.write "let () = ()\n" Path.(src_dir / Path.v "main.ml")) ~msg:"Failed to write runtime source";
      Result.expect (Fs.write "let () = ()\n" Path.(tests_dir / Path.v "demo_tests.ml")) ~msg:"Failed to write test source";
      Result.expect (Fs.write "fixture\n" Path.(tests_dir / Path.v "fixtures" / Path.v "sample.txt")) ~msg:"Failed to write fixture source";
      Result.expect (Fs.write "let () = ()\n" Path.(examples_dir / Path.v "example.ml")) ~msg:"Failed to write example source";
      Result.expect (Fs.write "let () = ()\n" Path.(bench_dir / Path.v "demo_bench.ml")) ~msg:"Failed to write bench source";
      let manifest = parse_manifest
        ~path:tmpdir
        ~relative_path:(Path.v "packages/demo")
        {|
[package]
name = "demo"
version = "0.1.0"
|}
      in
      let pkg = Riot_model.Package_manifest.realize ~intent:Riot_model.Package_manifest.Runtime manifest in
      if
        pkg.sources.src = [ Path.v "src/main.ml" ]
        && pkg.sources.native = []
        && pkg.sources.tests = []
        && pkg.sources.examples = []
        && pkg.sources.bench = []
        && binary_names pkg.binaries = [ "demo" ]
      then
        Ok ()
      else
        Error "expected runtime realization to load only runtime sources and binaries")

let test_manifest_realize_test_discovers_test_binaries_without_fixtures = fun _ctx ->
  with_tempdir "riot_model_package_manifest_test"
    (fun tmpdir ->
      let src_dir = Path.(tmpdir / Path.v "src") in
      let tests_dir = Path.(tmpdir / Path.v "tests") in
      let fixtures_dir = Path.(tests_dir / Path.v "fixtures") in
      let examples_dir = Path.(tmpdir / Path.v "examples") in
      Result.expect (Fs.create_dir_all src_dir) ~msg:"Failed to create src directory";
      Result.expect (Fs.create_dir_all tests_dir) ~msg:"Failed to create tests directory";
      Result.expect (Fs.create_dir_all fixtures_dir) ~msg:"Failed to create fixtures directory";
      Result.expect (Fs.create_dir_all examples_dir) ~msg:"Failed to create examples directory";
      Result.expect (Fs.write "let version = 1\n" Path.(src_dir / Path.v "demo.ml")) ~msg:"Failed to write library source";
      Result.expect (Fs.write "let () = ()\n" Path.(tests_dir / Path.v "demo_tests.ml")) ~msg:"Failed to write test source";
      Result.expect (Fs.write "let fixture = 1\n" Path.(fixtures_dir / Path.v "ignored.ml")) ~msg:"Failed to write fixture source";
      Result.expect (Fs.write "let () = ()\n" Path.(examples_dir / Path.v "example.ml")) ~msg:"Failed to write example source";
      let manifest = parse_manifest
        ~path:tmpdir
        ~relative_path:(Path.v "packages/demo")
        {|
[package]
name = "demo"
version = "0.1.0"

[lib]
path = "src/demo.ml"
|}
      in
      let pkg = Riot_model.Package_manifest.realize ~intent:Riot_model.Package_manifest.Test manifest in
      if
        pkg.sources.src = [ Path.v "src/demo.ml" ]
        && pkg.sources.tests = [ Path.v "tests/demo_tests.ml" ]
        && pkg.sources.examples = []
        && pkg.sources.bench = []
        && binary_names pkg.binaries = [ "demo_tests" ]
      then
        Ok ()
      else
        Error "expected test realization to load test modules but ignore support fixtures")

let tests = [
  Test.case
    "package manifest: from_toml keeps declared metadata only"
    test_manifest_from_toml_keeps_declared_metadata_only;
  Test.case
    "package manifest: runtime realization discovers runtime inputs only"
    test_manifest_realize_runtime_discovers_runtime_inputs_only;
  Test.case
    "package manifest: test realization ignores fixture support entries"
    test_manifest_realize_test_discovers_test_binaries_without_fixtures;
]

let name = "Riot Model Package Manifest Tests"

let () = Actors.run ~main:(Test.Cli.main ~name ~tests) ~args:Env.args ()
