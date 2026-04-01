open Std

type t

val filesystem: Registry_cache.t -> t

val in_memory:
  ?config:Sparse_index.config ->
  packages:Sparse_index.package_document list ->
  unit ->
  t

val read_config: t -> (Sparse_index.config option, string) result

val read_package_document:
  t ->
  package_name:string ->
  (Sparse_index.package_document option, string) result
