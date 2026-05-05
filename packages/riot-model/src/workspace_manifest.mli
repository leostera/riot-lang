open Std

type dependency_field =
  | Path
  | Source
  | Github
  | Ref
  | Version
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
  | DependencyCannotUseWorkspaceFlag of { dependency_name: string }
  | DependencyFieldMustBeString of {
      dependency_name: string;
      field: dependency_field;
    }
  | DependencyCannotSpecifySourceAndGithub of { dependency_name: string }
  | DependencyRefRequiresSource of { dependency_name: string }
  | BuiltinDependencyDoesNotSupportOverrides of { dependency_name: string }
  | BuiltinDependencyDoesNotSupportVersionRequirement of {
      dependency_name: string;
      requirement: string;
    }
  | DependencyMustBeStringOrTable of { dependency_name: string }
type error =
  | DependencySectionMustBeTable of { section_name: string }
  | DependencyError of dependency_error
type t = {
  name: string option;
  root: Path.t;
  target_dir_root: Path.t;
  source_ignore_patterns: string list;
  packages: Package_manifest.t list;
  dependencies: Package.dependency list;
  dev_dependencies: Package.dependency list;
  build_dependencies: Package.dependency list;
  profile_overrides: (string * Package.profile_override) list;
}
type manifest = {
  name: string option;
  members: Path.t list;
  source_ignore_patterns: string list;
  dependencies: Package.dependency list;
  dev_dependencies: Package.dependency list;
  build_dependencies: Package.dependency list;
  profile_overrides: (string * Package.profile_override) list;
  target_dir: string option;
}

val dependency_field_name: dependency_field -> string

val dependency_error_message: dependency_error -> string

val error_message: error -> string

val from_toml: Std.Data.Toml.value -> (manifest, error) result

val make:
  ?name:string ->
  root:Path.t ->
  packages:Package_manifest.t list ->
  ?dependencies:Package.dependency list ->
  ?dev_dependencies:Package.dependency list ->
  ?build_dependencies:Package.dependency list ->
  ?profile_overrides:(string * Package.profile_override) list ->
  ?source_ignore_patterns:string list ->
  ?target_dir:string ->
  unit ->
  t

val make_realized:
  ?name:string ->
  root:Path.t ->
  packages:Package.t list ->
  ?dependencies:Package.dependency list ->
  ?dev_dependencies:Package.dependency list ->
  ?build_dependencies:Package.dependency list ->
  ?profile_overrides:(string * Package.profile_override) list ->
  ?source_ignore_patterns:string list ->
  ?target_dir:string ->
  unit ->
  t

val dependencies_for_scope: Package.dependency_scope -> t -> Package.dependency list

val package_root: t -> Package_manifest.t -> Path.t

val find_package_for_path: t -> path:Path.t -> Package_manifest.t option

val realize_package: intent:Package.realization_intent -> t -> Package_manifest.t -> Package.t

val realize_packages: intent:Package.realization_intent -> t -> Package.t list

(**
   Get a unique project identifier for the workspace by replacing / with - in
   the root path
*)
val project_id: t -> string

(**
   Get a unique port number for the workspace server based on workspace root
   path. Returns a port in the dynamic/private range (49152-65535)
*)
val server_port: t -> int

(** Discover all package commands in the workspace by collecting commands from all packages *)
val discover_commands: t -> Package_command.t list

(** Find a command by name in the workspace *)
val find_command: t -> string -> Package_command.t option

(** Discover all package-provided riot-fix providers in the workspace *)
val discover_fix_providers: t -> Fix_provider.t list
