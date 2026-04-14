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
type checkout_status =
  | Cloned
  | Updated
  | Reused
type materialized = {
  source_locator: string;
  ref_: string;
  repository_root: Path.t;
  package_root: Path.t;
  checkout_status: checkout_status;
}
type error =
  | InvalidSourceSpec of { source: string; error: string }
  | UnsupportedSourceHost of { source: string; host: string }
  | CachedRepositoryInvalid of { path: Path.t }
  | PackageRootMissing of { path: Path.t }
  | GitCommandFailed of { command: string; status: int; stdout: string; stderr: string }
  | GitCommandSpawnFailed of { command: string; error: string }
val message: error -> string

val looks_like_remote_spec: string -> bool

val parse_spec: string -> (spec, error) result

val to_string: spec -> string

val parse_source_locator: string -> (locator, error) result

val sync_checkout:
  ?update:bool ->
  repo_dir:Path.t ->
  remote_url:string ->
  ref_:string ->
  unit ->
  (checkout_status, error) result

val materialize:
  ?update:bool -> source_locator:string -> ref_:string option -> unit -> (materialized, error) result
