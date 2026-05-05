open Std

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

val project_id: t -> string

val server_port: t -> int

val discover_commands: t -> Package_command.t list

val find_command: t -> string -> Package_command.t option

val discover_fix_providers: t -> Fix_provider.t list
