open Std

type dependency_source = Package.dependency_source

type dependency_scope = Package.dependency_scope

type dependency = Package.dependency

type publish_metadata = Package.publish_metadata

type binary = Package.binary

type library = Package.library

type realization_intent = Package.realization_intent =
  | Build
  | Runtime
  | Dev
  | Run
  | Test
  | Bench
  | Doc
  | Check

type profile_override = Package.profile_override

type compiler_config = Package.compiler_config

type foreign_dependency = Package.foreign_dependency

type error = Package.manifest_error

type t = Package.manifest_spec = {
  name: Package_name.t;
  path: Path.t;
  relative_path: Path.t;
  dependencies: dependency list;
  dev_dependencies: dependency list;
  build_dependencies: dependency list;
  foreign_dependencies: foreign_dependency list;
  declared_binaries: binary list;
  library: library option;
  compiler: compiler_config;
  commands: Package_command.t list;
  fix_providers: Fix_provider.t list;
  publish: publish_metadata;
}

let from_package = fun (pkg: Package.t) ->
  {
    name = pkg.name;
    path = pkg.path;
    relative_path = pkg.relative_path;
    dependencies = pkg.dependencies;
    dev_dependencies = pkg.dev_dependencies;
    build_dependencies = pkg.build_dependencies;
    foreign_dependencies = pkg.foreign_dependencies;
    declared_binaries = pkg.binaries;
    library = pkg.library;
    compiler = pkg.compiler;
    commands = pkg.commands;
    fix_providers = pkg.fix_providers;
    publish = pkg.publish;
  }

let is_workspace_member = fun manifest ->
  let rel_str = Path.to_string manifest.relative_path in
  not (String.starts_with ~prefix:"../" rel_str || Path.is_absolute manifest.relative_path)

let all_dependencies = fun manifest ->
  (manifest.dependencies @ manifest.dev_dependencies) @ manifest.build_dependencies

let from_toml = Package.parse_manifest_spec

let error_message = Package.manifest_error_message

let realize = Package.realize_manifest_spec
