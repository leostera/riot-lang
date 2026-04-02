open Std

type spec = {
  source_locator: string;
  ref_: string option;
}

type locator = {
  host: string;
  owner: string;
  repo: string;
  subdir: Path.t option;
}

type materialized = {
  source_locator: string;
  ref_: string;
  repository_root: Path.t;
  package_root: Path.t;
}

type error =
  | InvalidSourceSpec of { source: string; error: string }
  | UnsupportedSourceHost of { source: string; host: string }
  | CachedRepositoryInvalid of { path: Path.t }
  | PackageRootMissing of { path: Path.t }
  | GitCommandFailed of { command: string; status: int; stdout: string; stderr: string }
  | GitCommandSpawnFailed of { command: string; error: string }

val message: error -> string

val parse_spec: string -> (spec, error) result

val parse_source_locator: string -> (locator, error) result

val sync_checkout: repo_dir:Path.t -> remote_url:string -> ref_:string -> (unit, error) result

val materialize:
  source_locator:string ->
  ref_:string option ->
  unit ->
  (materialized, error) result
