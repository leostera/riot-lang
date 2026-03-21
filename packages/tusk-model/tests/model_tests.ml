open Std
module Test = Std.Test

let make_command () =
  Tusk_model.Package_command.
    {
      name = "demo";
      description = "Run the demo";
      package_name = "minttea";
      package_path = Path.v "packages/minttea";
      command_module = "Demo_cmd";
      command_source = Path.v "src/demo_cmd.ml";
      command_binary = Path.v "_build/debug/out/minttea/demo";
    }

let make_package () =
  let command = make_command () in
  Tusk_model.Package.
    {
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

let test_build_scope_drops_commands_and_runtime_outputs () =
  let pkg = make_package () in
  let projected = Tusk_model.Package.for_scope Tusk_model.Package.Build pkg in
  let no_commands = projected.commands = [] in
  let no_binaries = projected.binaries = [] in
  let no_library = projected.library = None in
  let no_runtime_deps = projected.dependencies = [] in
  let no_dev_deps = projected.dev_dependencies = [] in
  if no_commands && no_binaries && no_library && no_runtime_deps && no_dev_deps
  then Ok ()
  else Error "build scope should drop commands, binaries, library, and non-build deps"

let test_runtime_scope_keeps_commands () =
  let pkg = make_package () in
  let projected = Tusk_model.Package.for_scope Tusk_model.Package.Normal pkg in
  if List.length projected.commands = 1 && List.length projected.binaries = 1
  then Ok ()
  else Error "runtime scope should preserve package commands and normal binaries"

let test_dev_scope_keeps_only_dev_outputs () =
  let pkg = make_package () in
  let projected = Tusk_model.Package.for_scope Tusk_model.Package.Dev pkg in
  let no_library = projected.library = None in
  let no_commands = projected.commands = [] in
  let no_runtime_sources = projected.sources.src = [] && projected.sources.native = [] in
  let kept_dev_deps =
    List.map (fun (dep : Tusk_model.Package.dependency) -> dep.name) projected.dev_dependencies
    = [ "propane" ]
  in
  let kept_runtime_deps =
    List.map (fun (dep : Tusk_model.Package.dependency) -> dep.name) projected.dependencies
    = [ "std" ]
  in
  let no_normal_binaries =
    List.for_all
      (fun (bin : Tusk_model.Package.binary) ->
        String.starts_with ~prefix:"tests/" (Path.to_string bin.path)
        || String.starts_with ~prefix:"examples/" (Path.to_string bin.path)
        || String.starts_with ~prefix:"bench/" (Path.to_string bin.path))
      projected.binaries
  in
  if
    no_library && no_commands && no_runtime_sources && kept_dev_deps
    && kept_runtime_deps && no_normal_binaries
  then Ok ()
  else Error "dev scope should reuse runtime deps while keeping only dev outputs"

let tests =
  Test.
    [
      case "for_scope: build drops commands and runtime outputs"
        test_build_scope_drops_commands_and_runtime_outputs;
      case "for_scope: runtime keeps commands" test_runtime_scope_keeps_commands;
      case "for_scope: dev keeps only dev outputs"
        test_dev_scope_keeps_only_dev_outputs;
    ]

let name = "Tusk Model Tests"
let () = Miniriot.run ~main:(Test.Cli.main ~name ~tests) ~args:Env.args ()
