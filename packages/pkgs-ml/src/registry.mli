open Std

type t
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
val create_filesystem: registry_name:string -> ?tusk_home:Path.t -> unit -> (t, string) result

val filesystem: Registry_cache.t -> t

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
