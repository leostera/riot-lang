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

val message: error -> string

val validate_runtime_dependencies:
  package:Tusk_model.Package.t ->
  (unit, error) result

val create_artifact:
  package:Tusk_model.Package.t ->
  (string, error) result
