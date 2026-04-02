open Std
open Std.Data

type required_by = {
  package: string;
  path: Path.t option;
}
type t =
  | ManifestReadFailed of { manifest_path: Path.t; error: string }
  | ManifestParseFailed of { manifest_path: Path.t; error: string }
  | PathDependencyLoadFailed of { dependency_name: string; dependency_path: Path.t; error: t }
  | PathDependencyDecodeFailed of { dependency_name: string; manifest_path: Path.t; error: string }
  | RegistryLatestReleaseMissing of { package: string; latest_version: string }
  | PackageMetadataReadFailed of { package: string; registry: string; error: string }
  | PackageNotFound of { package: string; registry: string; required_by: required_by option }
  | LockfileReadFailed of { path: Path.t; error: string }
  | LockRefreshCheckFailed of { workspace_root: Path.t; error: string }
  | LockfileWriteFailed of { path: Path.t; error: string }
  | MaterializationFailed of { error: string }
  | ProjectionFailed of { error: string }
  | Unexpected of { error: string }
val headline: t -> string

val detail_lines: t -> string list

val message: t -> string

val to_json: t -> Json.t

val of_json: Json.t -> (t, string) result
