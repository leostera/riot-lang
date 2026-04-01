open Std
module Test = Std.Test

let make_command = fun () ->
  Tusk_model.Package_command.{
    name = "demo";
    description = "Run the demo";
    package_name = "minttea";
    package_path = Path.v "packages/minttea";
    command_module = "Demo_cmd";
    command_source = Path.v "src/demo_cmd.ml";
    command_binary = Path.v "_build/debug/out/minttea/demo";
  }

let make_package = fun () ->
  let command = make_command () in
  Tusk_model.Package.{
    name = "minttea";
    path = Path.v "packages/minttea";
    relative_path = Path.v "packages/minttea";
    dependencies = [ { name = "std"; source = Tusk_model.Package.Workspace } ];
    dev_dependencies = [ { name = "propane"; source = Tusk_model.Package.Workspace } ];
    build_dependencies = [ { name = "std"; source = Tusk_model.Package.Workspace } ];
    foreign_dependencies = [];
    binaries = [ { name = "demo-bin"; path = Path.v "src/demo_bin.ml" } ];
    library = Some { path = Path.v "src/minttea.ml" };
    sources =
      {
        src = [ Path.v "src/minttea.ml"; Path.v "src/demo_cmd.ml" ];
        native = [];
        tests = [ Path.v "tests/model_tests.ml" ];
        examples = [];
        bench = [];
      };
    compiler = { profile_overrides = []; target_overrides = [] };
    commands = [ command ];
    fix_providers = [];
  }

let with_tempdir = fun prefix fn ->
  match Fs.with_tempdir ~prefix fn with
  | Ok result -> result
  | Error _ -> Error "Tempdir creation failed"

let test_build_scope_drops_commands_and_runtime_outputs = fun () ->
  let pkg = make_package () in
  let projected = Tusk_model.Package.for_scope Tusk_model.Package.Build pkg in
  let no_commands = projected.commands = [] in
  let no_binaries = projected.binaries = [] in
  let no_library = projected.library = None in
  let no_runtime_deps = projected.dependencies = [] in
  let no_dev_deps = projected.dev_dependencies = [] in
  if no_commands && no_binaries && no_library && no_runtime_deps && no_dev_deps then
    Ok ()
  else
    Error "build scope should drop commands, binaries, library, and non-build deps"

let test_runtime_scope_keeps_commands = fun () ->
  let pkg = make_package () in
  let projected = Tusk_model.Package.for_scope Tusk_model.Package.Normal pkg in
  if List.length projected.commands = 1 && List.length projected.binaries = 1 then
    Ok ()
  else
    Error "runtime scope should preserve package commands and normal binaries"

let test_dev_scope_keeps_only_dev_outputs = fun () ->
  let pkg = make_package () in
  let projected = Tusk_model.Package.for_scope Tusk_model.Package.Dev pkg in
  let no_library = projected.library = None in
  let no_commands = projected.commands = [] in
  let no_runtime_sources = projected.sources.src = [] && projected.sources.native = [] in
  let kept_dev_deps = List.map (fun (dep: Tusk_model.Package.dependency) -> dep.name) projected.dev_dependencies
  = [ "propane" ] in
  let kept_runtime_deps = List.map (fun (dep: Tusk_model.Package.dependency) -> dep.name) projected.dependencies
  = [ "std" ] in
  let no_normal_binaries =
    List.for_all
      (fun (bin: Tusk_model.Package.binary) ->
        String.starts_with ~prefix:"tests/" (Path.to_string bin.path)
        || String.starts_with ~prefix:"examples/" (Path.to_string bin.path)
        || String.starts_with ~prefix:"bench/" (Path.to_string bin.path))
      projected.binaries
  in
  if
    no_library && no_commands && no_runtime_sources && kept_dev_deps && kept_runtime_deps && no_normal_binaries
  then
    Ok ()
  else
    Error "dev scope should reuse runtime deps while keeping only dev outputs"

let test_explicit_binaries_override_autodiscovery = fun () ->
  with_tempdir "tusk_model_package"
    (fun tmpdir ->
      let src_dir = Path.(tmpdir / Path.v "src") in
      let examples_dir = Path.(tmpdir / Path.v "examples") in
      Result.expect (Fs.create_dir_all src_dir) ~msg:"Failed to create src directory";
      Result.expect (Fs.create_dir_all examples_dir) ~msg:"Failed to create examples directory";
      Result.expect (Fs.write "let version = 1\n" Path.(src_dir / Path.v "demo.ml")) ~msg:"Failed to write library source";
      Result.expect
        (Fs.write "let () = ()\n" Path.(examples_dir / Path.v "test_https_httpbin.ml"))
        ~msg:"Failed to write explicit example";
      Result.expect (Fs.write "let () = ()\n" Path.(examples_dir / Path.v "simple_https.ml")) ~msg:"Failed to write autodiscovered example";
      let manifest =
        Std.Data.Toml.parse
          {|
[package]
name = "demo"
version = "0.1.0"

[lib]
path = "src/demo.ml"

[[bin]]
name = "test_https_httpbin"
path = "examples/test_https_httpbin.ml"
|}
        |> Result.expect ~msg:"Expected package TOML to parse"
      in
      let pkg = Tusk_model.Package.from_toml
        manifest
        ~workspace_deps:[]
        ~workspace_dev_deps:[]
        ~workspace_build_deps:[]
        ~path:tmpdir
        ~relative_path:(Path.v "packages/demo")
      |> Result.expect ~msg:"Expected package manifest to parse" in
      let binary_names = pkg.binaries |> List.map (fun (bin: Tusk_model.Package.binary) -> bin.name) in
      match binary_names with
      | ["test_https_httpbin";"simple_https"] -> Ok ()
      | _ ->
          Error (
            "expected explicit example binary to suppress autodiscovery \
              duplicate, got: [" ^ String.concat ", " binary_names ^ "]"
          ))

let test_workspace_fmt_ignore_parses = fun () ->
  let toml =
    Std.Data.Toml.parse
      {|
[workspace]
members = ["packages/demo"]

[tusk.fmt]
ignore = ["fixtures", "generated"]
|}
    |> Result.expect ~msg:"expected workspace TOML to parse"
  in
  let config = Tusk_model.Fmt_config.of_toml toml in
  Test.assert_equal ~expected:[ "fixtures"; "generated" ] ~actual:config.ignore_patterns;
  Ok ()

let test_package_fmt_ignore_loads = fun () ->
  with_tempdir "tusk_model_fmt_config"
    (fun tmpdir ->
      let manifest_path = Path.(tmpdir / Path.v "tusk.toml") in
      Fs.write
        {|
[package]
name = "demo"
version = "0.1.0"

[tusk.fmt]
ignore = ["tests/fixtures", "vendor"]
|}
        manifest_path |> Result.expect ~msg:"expected package manifest to write";
      let config = Tusk_model.Fmt_config.load manifest_path in
      Test.assert_equal ~expected:[ "tests/fixtures"; "vendor" ] ~actual:config.ignore_patterns;
      Ok ())

let test_legacy_fmt_ignore_still_loads = fun () ->
  let toml =
    Std.Data.Toml.parse
      {|
[fmt]
ignore = ["fixtures"]
|}
    |> Result.expect ~msg:"expected legacy fmt TOML to parse"
  in
  let config = Tusk_model.Fmt_config.of_toml toml in
  Test.assert_equal ~expected:[ "fixtures" ] ~actual:config.ignore_patterns;
  Ok ()

let tests =
  Test.[
    case "for_scope: build drops commands and runtime outputs" test_build_scope_drops_commands_and_runtime_outputs;
    case "for_scope: runtime keeps commands" test_runtime_scope_keeps_commands;
    case "for_scope: dev keeps only dev outputs" test_dev_scope_keeps_only_dev_outputs;
    case "package: explicit binaries suppress autodiscovery duplicates" test_explicit_binaries_override_autodiscovery;
    case "fmt config: workspace ignore parses" test_workspace_fmt_ignore_parses;
    case "fmt config: package ignore loads" test_package_fmt_ignore_loads;
    case "fmt config: legacy top-level fmt still loads" test_legacy_fmt_ignore_still_loads;
  ]

let name = "Tusk Model Tests"

let () = Miniriot.run ~main:(Test.Cli.main ~name ~tests) ~args:Env.args ()
