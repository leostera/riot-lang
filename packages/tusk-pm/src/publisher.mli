open Std

type error =
  | MissingManifest of { package_root: Path.t }
  | RuntimeDependencyNotPublishable of {
      package: string;
      dependency: string;
      reason: [
        | `PathOnly of Path.t
        | `WorkspaceOnly
      ];
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
  | RegistryPublishFailed of { locator: string; error: string }
  | CyclicWorkspacePublishOrder of { cycle: string list }

val message: error -> string

val validate_runtime_dependencies:
  package:Tusk_model.Package.t ->
  (unit, error) result

val create_artifact:
  package:Tusk_model.Package.t ->
  (string, error) result

val publish_from_locator:
  registry:Pkgs_ml.Registry.t ->
  package:Tusk_model.Package.t ->
  locator:string ->
  selector:string ->
  api_token:string ->
  (Pkgs_ml.Registry.published_release, error) result

val workspace_publish_order:
  packages:Tusk_model.Package.t list ->
  (Tusk_model.Package.t list, error) result
