open Std

type t
type fetch_response = {
  status_code: int;
  body: string;
}
type fetch
type published_artifact_location = {
  key: string;
  url: string option;
  cdn_url: string;
}
type published_record = {
  key: string;
  created: bool;
}
type published_materialization = {
  manifest_cached: bool;
  source_cached: bool;
}
type published_release = {
  package_locator: string option;
  source_url: string option;
  package_subdir: string option;
  selector: string;
  resolved_sha: string;
  package_name: string;
  package_version: string;
  manifest: published_artifact_location;
  source_archive: published_artifact_location;
  claim: published_record;
  release: published_record;
  materialization: published_materialization;
}
type release_file = {
  path: Path.t;
  contents: string;
}
type release_source = {
  package_name: string;
  version: string;
  manifest_toml: string;
  files: release_file list;
}
type materialize_result =
[
  `Materialized
  | `Already_present
]
val make_fetch:
  get:(Net.Uri.t -> (fetch_response, string) result) ->
  ?post:(Net.Uri.t -> headers:(string * string) list -> body:string -> (fetch_response, string) result) ->
  unit ->
  fetch

val create_filesystem: ?fetch:fetch -> registry_name:string -> ?tusk_home:Path.t -> unit -> (t, string) result

val filesystem: ?fetch:fetch -> Registry_cache.t -> t

val cache: t -> Registry_cache.t

val name: t -> string

val in_memory:
  ?config:Sparse_index.config ->
  cache:Registry_cache.t ->
  ?releases:release_source list ->
  packages:Sparse_index.package_document list ->
  unit ->
  t

val read_config: t -> (Sparse_index.config option, string) result

val read_package_document:
  t -> package_name:string -> (Sparse_index.package_document option, string) result

val materialize_release: t -> package_name:string -> version:string -> (materialize_result, string) result

val publish_artifact:
  t ->
  api_token:string ->
  artifact:string ->
  (published_release, string) result

val publish_from_locator:
  t ->
  locator:string ->
  selector:string ->
  api_token:string ->
  artifact:string ->
  (published_release, string) result
