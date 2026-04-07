open Std

(** Registry handle. *)
type t

(** Raw HTTP response returned by a fetch function. *)
type fetch_response = {
  status_code: int;
  body: string;
}

(** Fetch implementation used by the registry client. *)
type fetch

(** Published artifact location. *)
type published_artifact_location = {
  key: string;
  url: string;
}

(** Published record status returned by the registry. *)
type published_record = {
  key: string;
  created: bool;
}

(** Package search result. *)
type search_result = {
  package_name: string;
  latest_version: string;
  description: string option;
}

(** Materialization status for published assets. *)
type published_materialization = {
  manifest: bool;
  source: bool;
}

(** Result returned after publishing an artifact. *)
type published_release = {
  artifact_sha256: string;
  package_name: string;
  package_version: string;
  manifest: published_artifact_location;
  source_archive: published_artifact_location;
  claim: published_record;
  release: published_record;
  materialization: published_materialization;
}

(** Result returned after yanking a release. *)
type yanked_release = {
  package_name: string;
  package_version: string;
  yanked: bool;
  yanked_at: string option;
  yanked_by_github_login: string option;
}

(** One source file included in an in-memory release fixture. *)
type release_file = {
  path: Path.t;
  contents: string;
}

(** Release source payload used for in-memory registries and tests. *)
type release_source = {
  package_name: string;
  version: string;
  manifest_toml: string;
  files: release_file list;
}

(** Result of attempting to materialize a release into the cache. *)
type materialize_result =
[
  `Materialized
  | `Already_present
]

(** Build a fetch implementation from HTTP callbacks.

    Use this when you want to provide your own network layer, for example in
    tests or custom runtimes.
*)
val make_fetch:
  (** GET implementation. *)
  get:(Net.Uri.t -> (fetch_response, string) result) ->
  (** Optional POST implementation for mutating routes such as publish or yank. *)
  ?post:(Net.Uri.t -> headers:(string * string) list -> body:string -> (fetch_response, string) result) ->
  unit ->
  fetch

(** Set the default `X-Riot-Agent` header value used by registry requests. *)
val set_riot_agent: string option -> unit

(** Create a filesystem-backed registry client. *)
val create_filesystem:
  ?fetch:fetch -> registry_name:string -> ?riot_home:Path.t -> unit -> (t, string) result

(** Build a registry client from an existing cache. *)
val filesystem: ?fetch:fetch -> Registry_cache.t -> t

(** Return the backing cache. *)
val cache: t -> Registry_cache.t

(** Return the registry name. *)
val name: t -> string

(** Create an in-memory registry for tests or fixtures. *)
val in_memory:
  ?config:Sparse_index.config ->
  cache:Registry_cache.t ->
  ?releases:release_source list ->
  packages:Sparse_index.package_document list ->
  unit ->
  t

(** Read the sparse-index configuration, if cached or available. *)
val read_config: t -> (Sparse_index.config option, string) result

(** Read one package document by package name. *)
val read_package_document:
  t -> package_name:string -> (Sparse_index.package_document option, string) result

(** Search registry packages by query string. *)
val search_packages: t -> query:string -> ?limit:int -> unit -> (search_result list, string) result

(** Refresh one package document from the registry and update the cache. *)
val refresh_package_document:
  t -> package_name:string -> (Sparse_index.package_document option, string) result

(** Download and materialize one release into the local source cache. *)
val materialize_release: t -> package_name:string -> version:string -> (materialize_result, string) result

(** Publish one release artifact to the registry. *)
val publish_artifact: t -> api_token:string -> artifact:string -> (published_release, string) result

(** Yank one published release. *)
val yank_release:
  t -> api_token:string -> package_name:string -> version:string -> (yanked_release, string) result
