open Std

type t
type fetch_response = {
  status_code: int;
  body: string;
}
type fetch
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
val make_fetch: get:(Net.Uri.t -> (fetch_response, string) result) -> fetch

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
