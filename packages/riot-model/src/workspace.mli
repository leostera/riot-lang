open Std

type t = {
  name: string option;
  root: Path.t;
  target_dir_root: Path.t;
  packages: Package.t list;
  dependencies: Package.dependency list;
  dev_dependencies: Package.dependency list;
  build_dependencies: Package.dependency list;
  profile_overrides: (string * Package.profile_override) list;
}
type manifest = {
  name: string option;
  members: Path.t list;
  dependencies: Package.dependency list;
  dev_dependencies: Package.dependency list;
  build_dependencies: Package.dependency list;
  profile_overrides: (string * Package.profile_override) list;
  target_dir: string option;
}
val of_toml: Std.Data.Toml.value -> (manifest, string) result

val make:
  ?name:string ->
  root:Path.t ->
  packages:Package.t list ->
  ?dependencies:Package.dependency list ->
  ?dev_dependencies:Package.dependency list ->
  ?build_dependencies:Package.dependency list ->
  ?profile_overrides:(string * Package.profile_override) list ->
  ?target_dir:string ->
  unit ->
  t

val dependencies_for_scope: Package.dependency_scope -> t -> Package.dependency list

val package_root: t -> Package.t -> Path.t

val find_package_for_path: t -> path:Path.t -> Package.t option

(** Get a unique project identifier for the workspace by replacing / with - in
    the root path *)
val project_id: t -> string

(** Get a unique port number for the workspace server based on workspace root
    path. Returns a port in the dynamic/private range (49152-65535) *)
val server_port: t -> int

(** Discover all package commands in the workspace by collecting commands from all packages *)
val discover_commands: t -> Package_command.t list

(** Find a command by name in the workspace *)
val find_command: t -> string -> Package_command.t option

(** Discover all package-provided riot-fix providers in the workspace *)
val discover_fix_providers: t -> Fix_provider.t list
