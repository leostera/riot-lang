open Std

type dependency_source = {
  workspace: bool;
  builtin: bool;
  path: Path.t option;
  source_locator: string option;
  ref_: string option;
  version: Std.Version.requirement option;
}
type dependency_scope =
  | Normal
  | Dev
  | Build
type key
type dependency = {
  name: Package_name.t;
  source: dependency_source;
}
type resolved_dependency = {
  requirement: dependency;
  resolved_id: Lockfile.package_id;
}
type publish_metadata = {
  version: Std.Version.t option;
  description: string option;
  license: string option;
  is_public: bool option;
}
type binary = {
  name: string;
  path: Path.t;
}
type library = {
  path: Path.t;
}
type sources = {
  src: Path.t list;
  native: Path.t list;
  tests: Path.t list;
  examples: Path.t list;
  bench: Path.t list;
}
type dev_artifacts = { tests: bool; examples: bool; benches: bool }
type realization_intent =
  | Build
  | Runtime
  | Dev
  | Run
  | Test
  | Bench
  | Doc
  | Check
type target_platform = string
(* "macos", "linux", "windows", etc. *)
(** Re-export from Profile for convenience *)
type 'a override = 'a Profile.override
type profile_override = Profile.profile_override
(** Target-specific override *)
type target_override = {
  profile_override: Profile.profile_override option;
}
type compiler_config = {
  profile_overrides: (string * profile_override) list;
  target_overrides: (target_platform * target_override) list;
}
type foreign_dependency = {
  name: string;
  path: Path.t;
  inputs: Path.t list;
  build_cmd: string list;
  clean_cmd: string list option;
  test_cmd: string list option;
  outputs: Path.t list;
  env: (string * string) list;
}
type manifest_spec = {
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
type publish_field =
  | PublishVersion
  | PublishDescription
  | PublishLicense
  | PublishPublic
type dependency_field =
  | DependencyWorkspace
  | DependencyPath
  | DependencySource
  | DependencyGithub
  | DependencyRef
  | DependencyVersion
type publish_metadata_error =
  | PackageSectionMustBeTable
  | InvalidPackageVersion of {
      package_name: string;
      version: string;
      error: Std.Version.parse_error;
    }
  | NonStringPublishField of {
      package_name: string;
      field: publish_field;
    }
  | NonBooleanPublicFlag of { package_name: string }
type dependency_error =
  | InvalidDependencyName of {
      raw_name: string;
      error: Package_name.error;
    }
  | InvalidDependencyRequirement of {
      dependency_name: string;
      requirement: string;
      error: Std.Version.parse_error;
    }
  | NonBooleanWorkspaceFlag of { dependency_name: string }
  | NonStringDependencyField of {
      dependency_name: string;
      field: dependency_field;
    }
  | DependencyCannotSpecifySourceAndGithub of { dependency_name: string }
  | WorkspaceDependencyCannotSpecifyOverrides of { dependency_name: string }
  | DependencyRefRequiresSource of { dependency_name: string }
  | BuiltinDependencyCannotSpecifyOverrides of { dependency_name: string }
  | BuiltinDependencyVersionRequirementNotSupported of {
      dependency_name: string;
      requirement: string;
    }
  | DependencyMustBeStringOrTable of { dependency_name: string }
type manifest_error =
  | ManifestMustBeTable
  | InvalidPackageName of {
      raw_name: string;
      error: Package_name.error;
    }
  | InvalidPublishMetadata of publish_metadata_error
  | DependencySectionMustBeTable of { section_name: string }
  | InvalidDependency of dependency_error
type t = private {
  name: Package_name.t;
  path: Path.t;
  relative_path: Path.t;
  dependencies: dependency list;
  dev_dependencies: dependency list;
  build_dependencies: dependency list;
  foreign_dependencies: foreign_dependency list;
  binaries: binary list;
  library: library option;
  sources: sources;
  compiler: compiler_config;
  commands: Package_command.t list;
  fix_providers: Fix_provider.t list;
  publish: publish_metadata;
}
type resolved = {
  package: t;
  id: Lockfile.package_id;
  manifest_path: Path.t;
  materialized_root: Path.t;
  provenance: Lockfile.provenance;
  runtime_resolved: resolved_dependency list;
  build_resolved: resolved_dependency list;
  dev_resolved: resolved_dependency list;
}

val equal: t -> t -> bool

val is_workspace_member: t -> bool

(**
   Check if this package is a workspace member (not an external dependency).
   External dependencies have relative_path that escapes the workspace (starts with "../")
   or uses absolute paths.
*)
val validate_name: string -> (Package_name.t, Package_name.error) result

(**
   Validate a package name according to Riot naming conventions:
   - Must start with a lowercase letter
   - Can only contain lowercase letters, numbers, hyphens, and underscores
   - Cannot start or end with hyphens or underscores
   - Cannot be empty
*)
val is_builtin_dependency_name: string -> bool

val is_builtin_dependency: dependency -> bool

val from_toml:
  Std.Data.Toml.value ->
  workspace_deps:dependency list ->
  workspace_dev_deps:dependency list ->
  workspace_build_deps:dependency list ->
  path:Path.t ->
  relative_path:Path.t ->
  (t, manifest_error) result

val parse_manifest_spec:
  Std.Data.Toml.value ->
  workspace_deps:dependency list ->
  workspace_dev_deps:dependency list ->
  workspace_build_deps:dependency list ->
  path:Path.t ->
  relative_path:Path.t ->
  (manifest_spec, manifest_error) result

val manifest_error_message: manifest_error -> string

val realize_manifest_spec: intent:realization_intent -> manifest_spec -> t

val of_manifest_spec: manifest_spec -> t

val to_json: t -> Std.Data.Json.t

val from_json: Std.Data.Json.t -> (t, string) result

val make:
  name:Package_name.t ->
  path:Path.t ->
  relative_path:Path.t ->
  ?dependencies:dependency list ->
  ?dev_dependencies:dependency list ->
  ?build_dependencies:dependency list ->
  ?foreign_dependencies:foreign_dependency list ->
  ?binaries:binary list ->
  ?library:library ->
  ?sources:sources ->
  ?compiler:compiler_config ->
  ?commands:Package_command.t list ->
  ?fix_providers:Fix_provider.t list ->
  ?publish:publish_metadata ->
  unit ->
  t

val scan_sources: package_path:Path.t -> ?excluded_relpaths:Path.t list -> unit -> sources

val synthetic: name:Package_name.t -> path:Path.t -> relative_path:Path.t -> t

val root_module_name: t -> string

val key_of_string: string -> key

val key_to_string: key -> string

val key_equal: key -> key -> bool

val key_compare: key -> key -> Order.t

val dependencies_for_scope: dependency_scope -> t -> dependency list

val scope_of_binary_name: t -> binary_name:string -> dependency_scope option

val binaries_for_scope: ?dev_artifacts:dev_artifacts -> dependency_scope -> t -> binary list

val for_scope: ?dev_artifacts:dev_artifacts -> dependency_scope -> t -> t

val build_graph_dependencies: t -> dependency list

val all_dependencies: t -> dependency list

val resolve:
  package:t ->
  lock_package:Lockfile.package ->
  manifest_path:Path.t ->
  materialized_root:Path.t ->
  (resolved, string) result

(** Hash package metadata into a Sha256 hasher state *)
val hash: Crypto.Sha256.state -> t -> unit
