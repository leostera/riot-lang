open Std
open Std.Data

type required_by = {
  package: string;
  path: Path.t option;
}
type t =
  | ManifestReadFailed of {
      manifest_path: Path.t;
      error: string;
    }
  | ManifestParseFailed of {
      manifest_path: Path.t;
      error: string;
    }
  | PathDependencyLoadFailed of {
      dependency_name: string;
      dependency_path: Path.t;
      error: t;
    }
  | PathDependencyDecodeFailed of {
      dependency_name: string;
      manifest_path: Path.t;
      error: string;
    }
  | SourceDependencyLoadFailed of {
      dependency_name: string;
      source_locator: string;
      ref_: string option;
      error: string;
    }
  | SourceDependencyDecodeFailed of {
      dependency_name: string;
      manifest_path: Path.t;
      error: string;
    }
  | RegistryLatestReleaseMissing of { package: string; latest_version: string }
  | RegistryReleaseYanked of {
      package: string;
      registry: string;
      version: string;
      required_by: required_by option;
    }
  | PackageMetadataReadFailed of { package: string; registry: string; error: string }
  | PackageNotFound of {
      package: string;
      registry: string;
      required_by: required_by option;
    }
  | RegistryVersionNotFound of {
      package: string;
      registry: string;
      requirement: string;
      available_versions: string list;
      required_by: required_by option;
    }
  | LockfileReadFailed of {
      path: Path.t;
      error: string;
    }
  | LockRefreshCheckFailed of {
      workspace_root: Path.t;
      error: string;
    }
  | LockfileWriteFailed of {
      path: Path.t;
      error: string;
    }
  | MaterializationFailed of { error: string }
  | ProjectionFailed of { error: string }
  | Unexpected of { error: string }

val headline: t -> string

val detail_lines: t -> string list

val message: t -> string

val to_json: t -> Json.t

val from_json: Json.t -> (t, string) result
