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
type t = Package.manifest_spec

val from_package: Package.t -> t

val is_workspace_member: t -> bool

val all_dependencies: t -> dependency list

val from_toml:
  Std.Data.Toml.value ->
  workspace_deps:dependency list ->
  workspace_dev_deps:dependency list ->
  workspace_build_deps:dependency list ->
  path:Path.t ->
  relative_path:Path.t ->
  (t, error) result

val error_message: error -> string

val realize:
  ?source_ignore_patterns:string list ->
  ?source_scan_concurrency:int ->
  intent:realization_intent ->
  t ->
  Package.t
