open Std

(** Sparse-index configuration document. *)
type config = {
  schema_version: int;
  kind: string;
  package_path_strategy: string;
  index_base_url: string;
  artifact_base_url: string;
}
(** Dependency entry from a package document. *)
type dependency = {
  name: string;
  raw: Data.Json.t;
}
(** Published release entry in a package document. *)
type release = {
  version: string;
  published_at: string;
  canonical_locator: string;
  repo_url: string;
  subdir: string;
  artifact_sha256: string;
  description: string option;
  license: string option;
  homepage: string option;
  repository: string option;
  root_module: string option;
  categories: string list;
  keywords: string list;
  manifest_key: string;
  source_key: string;
  dependencies: dependency list;
  yanked: bool;
  yanked_at: string option;
  yanked_by_github_login: string option;
}
(** Sparse-index package document. *)
type package_document = {
  schema_version: int;
  name: string;
  latest: string;
  updated_at: string;
  releases: release list;
}

(** Normalize a package name for sparse-index lookup. *)
val normalized_name: string -> string

(** Return the sparse-index directory prefix for a package. *)
val package_prefix: string -> Path.t

(** Return the full sparse-index relative path for a package document. *)
val package_relpath: string -> Path.t

(** Return the bootstrap configuration URL for a registry. *)
val bootstrap_config_url: registry_name:string -> (Net.Uri.t, string) result

(** Return the package-document URL for a package. *)
val package_document_url: config -> package_name:string -> (Net.Uri.t, string) result

(** Return the source archive URL for a release. *)
val release_source_url: config -> release -> (Net.Uri.t, string) result

(** Return the cache path for one package document. *)
val package_cache_path: Registry_cache.t -> package_name:string -> Path.t

(** Return the cache path for the registry configuration document. *)
val config_cache_path: Registry_cache.t -> Path.t

(** Decode a sparse-index config from JSON. *)
val config_of_json: Data.Json.t -> (config, string) result

(** Decode a sparse-index config from a string. *)
val config_of_string: string -> (config, string) result

(** Decode a package document from JSON. *)
val package_document_of_json: Data.Json.t -> (package_document, string) result

(** Decode a package document from a string. *)
val package_document_of_string: string -> (package_document, string) result

(** Read the cached sparse-index config, if present. *)
val read_cached_config: Registry_cache.t -> (config option, string) result

(** Read one cached package document, if present. *)
val read_cached_package_document:
  Registry_cache.t -> package_name:string -> (package_document option, string) result

(** Write the sparse-index config source into the cache. *)
val write_cached_config: Registry_cache.t -> source:string -> (unit, string) result

(** Write a package document source into the cache. *)
val write_cached_package_document:
  Registry_cache.t -> package_name:string -> source:string -> (unit, string) result
