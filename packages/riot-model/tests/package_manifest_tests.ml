open Std

module Test = Std.Test

let package_name = fun name ->
  Riot_model.Package_name.from_string name
  |> Result.expect ~msg:("Expected valid package name: " ^ name)

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

let sort_paths = fun paths -> List.sort paths ~compare:Path.compare

let test_manifest_from_toml_keeps_declared_metadata_only = fun _ctx ->
  with_tempdir
    "riot_model_package_manifest"
    (fun tmpdir ->
      let src_dir = Path.(tmpdir / Path.v "src") in
      let tests_dir = Path.(tmpdir / Path.v "tests") in
      Result.expect (Fs.create_dir_all src_dir) ~msg:"Failed to create src directory";
      Result.expect (Fs.create_dir_all tests_dir) ~msg:"Failed to create tests directory";
      Result.expect
        (Fs.write "let () = ()\n" Path.(src_dir / Path.v "main.ml"))
        ~msg:"Failed to write runtime source";
      Result.expect
        (Fs.write "let () = ()\n" Path.(tests_dir / Path.v "demo_tests.ml"))
        ~msg:"Failed to write test source";
      let manifest =
        parse_manifest
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
        Riot_model.Package_name.equal manifest.name (package_name "demo")
        && manifest.declared_binaries
        = [ Riot_model.Package.{ name = "custom"; path = Path.v "examples/custom.ml" } ]
      then
        Ok ()
      else
        Error "expected package manifest to keep only declared binaries and metadata")

let test_manifest_from_toml_returns_typed_dependency_errors = fun _ctx ->
  let manifest =
    Std.Data.Toml.parse
      {|
[package]
name = "demo"
version = "0.1.0"

[dependencies]
std = "not-a-semver-range"
|}
    |> Result.expect ~msg:"Expected package TOML to parse"
  in
  match Riot_model.Package_manifest.from_toml
    manifest
    ~workspace_deps:[]
    ~workspace_dev_deps:[]
    ~workspace_build_deps:[]
    ~path:(Path.v "/tmp/demo")
    ~relative_path:(Path.v "packages/demo") with
  | Error (
    Riot_model.Package.InvalidDependency (
      Riot_model.Package.InvalidDependencyRequirement { dependency_name; requirement; _ }
    )
  ) when String.equal dependency_name "std" && String.equal requirement "not-a-semver-range" ->
      Ok ()
  | Error err ->
      Error ("expected typed dependency requirement error, got "
      ^ Riot_model.Package_manifest.error_message err)
  | Ok _ -> Error "expected invalid dependency requirement to fail package manifest parsing"

let test_manifest_realize_runtime_discovers_runtime_inputs_only = fun _ctx ->
  with_tempdir
    "riot_model_package_manifest_runtime"
    (fun tmpdir ->
      let src_dir = Path.(tmpdir / Path.v "src") in
      let tests_dir = Path.(tmpdir / Path.v "tests") in
      let examples_dir = Path.(tmpdir / Path.v "examples") in
      let bench_dir = Path.(tmpdir / Path.v "bench") in
      Result.expect (Fs.create_dir_all src_dir) ~msg:"Failed to create src directory";
      Result.expect (Fs.create_dir_all tests_dir) ~msg:"Failed to create tests directory";
      Result.expect
        (Fs.create_dir_all Path.(tests_dir / Path.v "fixtures"))
        ~msg:"Failed to create test fixtures directory";
      Result.expect (Fs.create_dir_all examples_dir) ~msg:"Failed to create examples directory";
      Result.expect (Fs.create_dir_all bench_dir) ~msg:"Failed to create bench directory";
      Result.expect
        (Fs.write "let () = ()\n" Path.(src_dir / Path.v "main.ml"))
        ~msg:"Failed to write runtime source";
      Result.expect
        (Fs.write "let () = ()\n" Path.(tests_dir / Path.v "demo_tests.ml"))
        ~msg:"Failed to write test source";
      Result.expect
        (Fs.write "fixture\n" Path.(tests_dir / Path.v "fixtures" / Path.v "sample.txt"))
        ~msg:"Failed to write fixture source";
      Result.expect
        (Fs.write "let () = ()\n" Path.(examples_dir / Path.v "example.ml"))
        ~msg:"Failed to write example source";
      Result.expect
        (Fs.write "let () = ()\n" Path.(bench_dir / Path.v "demo_bench.ml"))
        ~msg:"Failed to write bench source";
      let manifest =
        parse_manifest
          ~path:tmpdir
          ~relative_path:(Path.v "packages/demo")
          {|
[package]
name = "demo"
version = "0.1.0"
|}
      in
      let pkg =
        Riot_model.Package_manifest.realize ~intent:Riot_model.Package_manifest.Runtime manifest
      in
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

let test_manifest_realize_runtime_keeps_nested_runtime_modules = fun _ctx ->
  with_tempdir
    "riot_model_package_manifest_runtime_nested"
    (fun tmpdir ->
      let src_dir = Path.(tmpdir / Path.v "src") in
      let net_dir = Path.(src_dir / Path.v "net") in
      Result.expect
        (Fs.create_dir_all net_dir)
        ~msg:"Failed to create nested runtime source directory";
      Result.expect
        (Fs.write "let version = 1\n" Path.(src_dir / Path.v "demo.ml"))
        ~msg:"Failed to write library source";
      Result.expect
        (Fs.write "type t\n" Path.(net_dir / Path.v "udp_socket.mli"))
        ~msg:"Failed to write udp_socket interface";
      Result.expect
        (Fs.write "type t = unit\n" Path.(net_dir / Path.v "udp_socket.ml"))
        ~msg:"Failed to write udp_socket implementation";
      Result.expect
        (Fs.write
          "type handler = socket:Udp_socket.t -> bytes -> unit\n"
          Path.(net_dir / Path.v "udp_server.mli"))
        ~msg:"Failed to write udp_server interface";
      Result.expect
        (Fs.write
          "type handler = socket:Udp_socket.t -> bytes -> unit\n"
          Path.(net_dir / Path.v "udp_server.ml"))
        ~msg:"Failed to write udp_server implementation";
      let manifest =
        parse_manifest
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
      let expected =
        [
          Path.v "src/demo.ml";
          Path.v "src/net/udp_server.ml";
          Path.v "src/net/udp_server.mli";
          Path.v "src/net/udp_socket.ml";
          Path.v "src/net/udp_socket.mli";
        ]
        |> List.sort ~compare:Path.compare
      in
      let rec run iteration =
        if iteration = 0 then
          Ok ()
        else
          let pkg =
            Riot_model.Package_manifest.realize ~intent:Riot_model.Package_manifest.Runtime manifest
          in
          let actual =
            pkg.sources.src
            |> List.sort ~compare:Path.compare
          in
          if actual = expected then
            run (iteration - 1)
          else
            Error ("expected nested runtime realization sources ["
            ^ String.concat ", " (List.map expected ~fn:Path.to_string)
            ^ "] but got ["
            ^ String.concat ", " (List.map actual ~fn:Path.to_string)
            ^ "]")
      in
      run 25)

let test_manifest_realize_runtime_stays_complete_across_repeated_parallel_scans = fun _ctx ->
  with_tempdir
    "riot_model_package_manifest_runtime_parallel_complete"
    (fun tmpdir ->
      let src_dir = Path.(tmpdir / Path.v "src") in
      let net_dir = Path.(src_dir / Path.v "net") in
      let unicode_dir = Path.(src_dir / Path.v "unicode") in
      let worker_pool_dir = Path.(src_dir / Path.v "worker_pool") in
      let native_dir = Path.(tmpdir / Path.v "native") in
      Result.expect (Fs.create_dir_all net_dir) ~msg:"Failed to create nested net source directory";
      Result.expect
        (Fs.create_dir_all unicode_dir)
        ~msg:"Failed to create nested unicode source directory";
      Result.expect
        (Fs.create_dir_all worker_pool_dir)
        ~msg:"Failed to create nested worker_pool source directory";
      Result.expect (Fs.create_dir_all native_dir) ~msg:"Failed to create native source directory";
      let runtime_files = [
        (Path.v "src/main.ml", "let () = ()\n");
        (Path.v "src/app.ml", "let version = 1\n");
        (Path.v "src/net/udp_socket.mli", "type t\n");
        (Path.v "src/net/udp_socket.ml", "type t = unit\n");
        (Path.v "src/net/udp_server.mli", "type handler = socket:Udp_socket.t -> bytes -> unit\n");
        (Path.v "src/net/udp_server.ml", "type handler = socket:Udp_socket.t -> bytes -> unit\n");
        (Path.v "src/unicode/utf8.mli", "val decode : string -> string\n");
        (Path.v "src/unicode/utf8.ml", "let decode value = value\n");
        (Path.v "src/unicode/utf16.mli", "val encode : string -> string\n");
        (Path.v "src/unicode/utf16.ml", "let encode value = value\n");
        (Path.v "src/unicode/segmentation.mli", "val words : string -> string list\n");
        (Path.v "src/unicode/segmentation.ml", "let words value = [value]\n");
        (Path.v "src/worker_pool/coordinator.mli", "type t\n");
        (Path.v "src/worker_pool/coordinator.ml", "type t = unit\n");
        (Path.v "src/worker_pool/dynamic.mli", "val create : unit -> Coordinator.t\n");
        (Path.v "src/worker_pool/dynamic.ml", "let create () = Coordinator\n");
        (Path.v "native/runtime.c", "int runtime(void) { return 1; }\n");
        (Path.v "native/runtime_stubs.c", "int runtime_stubs(void) { return 1; }\n");
        (Path.v "native/runtime.h", "#pragma once\n");
      ]
      in
      List.for_each
        runtime_files
        ~fn:(fun (rel_path, contents) ->
          Result.expect
            (Fs.write contents Path.(tmpdir / rel_path))
            ~msg:("Failed to write runtime file " ^ Path.to_string rel_path));
      let manifest =
        parse_manifest
          ~path:tmpdir
          ~relative_path:(Path.v "packages/demo")
          {|
[package]
name = "demo"
version = "0.1.0"
|}
      in
      let expected_src =
        runtime_files
        |> List.map ~fn:(fun (path, _) -> path)
        |> List.filter ~fn:(fun path -> String.starts_with ~prefix:"src/" (Path.to_string path))
        |> sort_paths
      in
      let expected_native =
        runtime_files
        |> List.map ~fn:(fun (path, _) -> path)
        |> List.filter ~fn:(fun path -> String.starts_with ~prefix:"native/" (Path.to_string path))
        |> sort_paths
      in
      let rec run iteration =
        if iteration = 0 then
          Ok ()
        else
          let pkg =
            Riot_model.Package_manifest.realize ~intent:Riot_model.Package_manifest.Runtime manifest
          in
          let actual_src = sort_paths pkg.sources.src in
          let actual_native = sort_paths pkg.sources.native in
          if actual_src = expected_src && actual_native = expected_native then
            run (iteration - 1)
          else
            Error ("expected repeated runtime realization sources src=["
            ^ String.concat ", " (List.map expected_src ~fn:Path.to_string)
            ^ "] native=["
            ^ String.concat ", " (List.map expected_native ~fn:Path.to_string)
            ^ "] but got src=["
            ^ String.concat ", " (List.map actual_src ~fn:Path.to_string)
            ^ "] native=["
            ^ String.concat ", " (List.map actual_native ~fn:Path.to_string)
            ^ "]")
      in
      run 25)

let test_manifest_realize_build_skips_source_loading = fun _ctx ->
  with_tempdir
    "riot_model_package_manifest_build"
    (fun tmpdir ->
      let src_dir = Path.(tmpdir / Path.v "src") in
      let tests_dir = Path.(tmpdir / Path.v "tests") in
      let examples_dir = Path.(tmpdir / Path.v "examples") in
      Result.expect (Fs.create_dir_all src_dir) ~msg:"Failed to create src directory";
      Result.expect (Fs.create_dir_all tests_dir) ~msg:"Failed to create tests directory";
      Result.expect (Fs.create_dir_all examples_dir) ~msg:"Failed to create examples directory";
      Result.expect
        (Fs.write "let () = ()\n" Path.(src_dir / Path.v "main.ml"))
        ~msg:"Failed to write runtime source";
      Result.expect
        (Fs.write "let () = ()\n" Path.(tests_dir / Path.v "demo_tests.ml"))
        ~msg:"Failed to write test source";
      Result.expect
        (Fs.write "let () = ()\n" Path.(examples_dir / Path.v "example.ml"))
        ~msg:"Failed to write example source";
      let manifest =
        parse_manifest
          ~path:tmpdir
          ~relative_path:(Path.v "packages/demo")
          {|
[package]
name = "demo"
version = "0.1.0"

[[bin]]
name = "custom"
path = "src/main.ml"
|}
      in
      let pkg =
        Riot_model.Package_manifest.realize ~intent:Riot_model.Package_manifest.Build manifest
      in
      if
        pkg.sources.src = []
        && pkg.sources.native = []
        && pkg.sources.tests = []
        && pkg.sources.examples = []
        && pkg.sources.bench = []
        && pkg.binaries = []
      then
        Ok ()
      else
        Error "expected build realization to skip loading sources and binaries")

let test_manifest_realize_test_discovers_test_binaries_without_fixtures = fun _ctx ->
  with_tempdir
    "riot_model_package_manifest_test"
    (fun tmpdir ->
      let src_dir = Path.(tmpdir / Path.v "src") in
      let tests_dir = Path.(tmpdir / Path.v "tests") in
      let fixtures_dir = Path.(tests_dir / Path.v "fixtures") in
      let examples_dir = Path.(tmpdir / Path.v "examples") in
      Result.expect (Fs.create_dir_all src_dir) ~msg:"Failed to create src directory";
      Result.expect (Fs.create_dir_all tests_dir) ~msg:"Failed to create tests directory";
      Result.expect (Fs.create_dir_all fixtures_dir) ~msg:"Failed to create fixtures directory";
      Result.expect (Fs.create_dir_all examples_dir) ~msg:"Failed to create examples directory";
      Result.expect
        (Fs.write "let version = 1\n" Path.(src_dir / Path.v "demo.ml"))
        ~msg:"Failed to write library source";
      Result.expect
        (Fs.write "let () = ()\n" Path.(tests_dir / Path.v "demo_tests.ml"))
        ~msg:"Failed to write test source";
      Result.expect
        (Fs.write "let fixture = 1\n" Path.(fixtures_dir / Path.v "ignored.ml"))
        ~msg:"Failed to write fixture source";
      Result.expect
        (Fs.write "let () = ()\n" Path.(examples_dir / Path.v "example.ml"))
        ~msg:"Failed to write example source";
      let manifest =
        parse_manifest
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
      let pkg =
        Riot_model.Package_manifest.realize ~intent:Riot_model.Package_manifest.Test manifest
      in
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

let test_manifest_realize_doc_excludes_executable_sources = fun _ctx ->
  with_tempdir
    "riot_model_package_manifest_doc"
    (fun tmpdir ->
      let src_dir = Path.(tmpdir / Path.v "src") in
      Result.expect (Fs.create_dir_all src_dir) ~msg:"Failed to create src directory";
      Result.expect
        (Fs.write "let parse = fun value -> value\n" Path.(src_dir / Path.v "synlike.ml"))
        ~msg:"Failed to write library source";
      Result.expect
        (Fs.write "val parse: string -> string\n" Path.(src_dir / Path.v "synlike.mli"))
        ~msg:"Failed to write library interface";
      Result.expect
        (Fs.write "let main ~args:_ = Synlike.parse \"ok\"\n" Path.(src_dir / Path.v "main.ml"))
        ~msg:"Failed to write binary source";
      Result.expect
        (Fs.write "let main ~args:_ = ()\n" Path.(src_dir / Path.v "demo_cmd.ml"))
        ~msg:"Failed to write command source";
      let manifest =
        parse_manifest
          ~path:tmpdir
          ~relative_path:(Path.v "packages/synlike")
          {|
[package]
name = "synlike"
version = "0.1.0"

[lib]
path = "src/synlike.ml"

[[bin]]
name = "synlike"
path = "src/main.ml"

[[command]]
name = "demo"
help = "Run the demo"
path = "src/demo_cmd.ml"
|}
      in
      let pkg =
        Riot_model.Package_manifest.realize ~intent:Riot_model.Package_manifest.Doc manifest
      in
      let src = sort_paths pkg.sources.src in
      if
        src = [ Path.v "src/synlike.ml"; Path.v "src/synlike.mli" ]
        && pkg.binaries = []
        && pkg.commands = []
      then
        Ok ()
      else
        Error "expected doc realization to keep library sources but exclude binary and command sources")

let test_manifest_to_package_preserves_declared_binaries_without_loading_sources = fun _ctx ->
  with_tempdir
    "riot_model_package_manifest_package"
    (fun tmpdir ->
      let src_dir = Path.(tmpdir / Path.v "src") in
      let tests_dir = Path.(tmpdir / Path.v "tests") in
      Result.expect (Fs.create_dir_all src_dir) ~msg:"Failed to create src directory";
      Result.expect (Fs.create_dir_all tests_dir) ~msg:"Failed to create tests directory";
      Result.expect
        (Fs.write "let () = ()\n" Path.(src_dir / Path.v "main.ml"))
        ~msg:"Failed to write runtime source";
      Result.expect
        (Fs.write "let () = ()\n" Path.(tests_dir / Path.v "demo_tests.ml"))
        ~msg:"Failed to write test source";
      let manifest =
        parse_manifest
          ~path:tmpdir
          ~relative_path:(Path.v "packages/demo")
          {|
[package]
name = "demo"
version = "0.1.0"

[[bin]]
name = "demo-example"
path = "examples/demo_example.ml"

[[bin]]
name = "demo-tests"
path = "tests/demo_tests.ml"
|}
      in
      let pkg = Riot_model.Package.of_manifest_spec manifest in
      if
        binary_names pkg.binaries = [ "demo-example"; "demo-tests" ]
        && pkg.sources.src = []
        && pkg.sources.native = []
        && pkg.sources.tests = []
        && pkg.sources.examples = []
        && pkg.sources.bench = []
      then
        Ok ()
      else
        Error "expected manifest conversion to preserve declared binaries without scanning sources")

let tests = [
  Test.case
    "package manifest: from_toml keeps declared metadata only"
    test_manifest_from_toml_keeps_declared_metadata_only;
  Test.case
    "package manifest: from_toml returns typed dependency errors"
    test_manifest_from_toml_returns_typed_dependency_errors;
  Test.case
    "package manifest: runtime realization discovers runtime inputs only"
    test_manifest_realize_runtime_discovers_runtime_inputs_only;
  Test.case
    "package manifest: runtime realization keeps nested runtime modules"
    test_manifest_realize_runtime_keeps_nested_runtime_modules;
  Test.case
    "package manifest: runtime realization stays complete across repeated parallel scans"
    test_manifest_realize_runtime_stays_complete_across_repeated_parallel_scans;
  Test.case
    "package manifest: build realization skips source loading"
    test_manifest_realize_build_skips_source_loading;
  Test.case
    "package manifest: test realization ignores fixture support entries"
    test_manifest_realize_test_discovers_test_binaries_without_fixtures;
  Test.case
    "package manifest: doc realization excludes executable sources"
    test_manifest_realize_doc_excludes_executable_sources;
  Test.case
    "package manifest: to package preserves declared binaries without loading sources"
    test_manifest_to_package_preserves_declared_binaries_without_loading_sources;
]

let name = "Riot Model Package Manifest Tests"

let main ~args = Test.Cli.main ~name ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
