open Std

type error =
  | MissingPublishVersion of { package: string }
  | MissingPublishDescription of { package: string }
  | MissingPublishLicense of { package: string }
  | PackageNotPublic of { package: string }
  | MissingManifest of { package_root: Path.t }
  | RuntimeDependencyNotPublishable of {
      package: string;
      dependency: string;
      reason: [
        | `PathOnly of Path.t
        | `WorkspaceOnly
        | `MissingVersionOrPath
      ];
    }
  | RuntimeDependencyRegistryLookupFailed of {
      package: string;
      dependency: string;
      registry: string;
      error: string;
    }
  | RuntimeDependencyNotFoundInRegistry of {
      package: string;
      dependency: string;
      registry: string;
    }
  | SymlinkNotAllowed of { path: Path.t }
  | UnsupportedEntry of { path: Path.t; kind: string }
  | DirectoryReadFailed of { path: Path.t; error: string }
  | MetadataReadFailed of { path: Path.t; error: string }
  | ArtifactReadFailed of { path: Path.t; error: string }
  | TarCommandFailed of {
      command: string;
      status: int;
      stdout: string;
      stderr: string;
    }
  | TarCommandSpawnFailed of { command: string; error: string }
  | GitProvenanceFailed of Git_provenance.error
  | RegistryPublishFailed of { locator: string; error: string }
  | CyclicWorkspacePublishOrder of { cycle: string list }

type prepared_publish = {
  package: Tusk_model.Package.t;
  version: Std.Version.t;
  locator: string;
  selector: string;
  artifact_path: Path.t;
}

val message: error -> string

val validate_publish_metadata:
  package:Tusk_model.Package.t ->
  (Std.Version.t, error) result

val validate_runtime_dependencies:
  package:Tusk_model.Package.t ->
  (unit, error) result

val validate_registry_dependencies:
  registry:Pkgs_ml.Registry.t ->
  publishing_workspace_packages:string list ->
  package:Tusk_model.Package.t ->
  (unit, error) result

val create_artifact:
  target_dir_root:Path.t ->
  package:Tusk_model.Package.t ->
  version:Std.Version.t ->
  (Path.t, error) result

val prepare_publish:
  registry:Pkgs_ml.Registry.t ->
  target_dir_root:Path.t ->
  publishing_workspace_packages:string list ->
  package:Tusk_model.Package.t ->
  (prepared_publish, error) result

val publish_prepared:
  registry:Pkgs_ml.Registry.t ->
  api_token:string ->
  prepared_publish ->
  (Pkgs_ml.Registry.published_release, error) result

val publish_from_locator:
  registry:Pkgs_ml.Registry.t ->
  target_dir_root:Path.t ->
  package:Tusk_model.Package.t ->
  locator:string ->
  selector:string ->
  api_token:string ->
  (Pkgs_ml.Registry.published_release, error) result

val publish:
  registry:Pkgs_ml.Registry.t ->
  target_dir_root:Path.t ->
  publishing_workspace_packages:string list ->
  package:Tusk_model.Package.t ->
  api_token:string ->
  (Pkgs_ml.Registry.published_release, error) result

val workspace_publish_order:
  packages:Tusk_model.Package.t list ->
  (Tusk_model.Package.t list, error) result
