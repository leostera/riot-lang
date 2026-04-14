open Std

type package_status =
  | Built of Riot_store.Artifact.t
  | Cached of Riot_store.Artifact.t
  | Skipped of string
  | Failed of string

type package_result

type t

val of_build_results: Riot_executor.Package_builder.build_result list -> t

val packages: t -> package_result list

val find_package: t -> Riot_model.Package_name.t -> package_result option

val package_name: package_result -> Riot_model.Package_name.t

val package_status: package_result -> package_status

val package_artifact: package_result -> Riot_store.Artifact.t option

val find_export:
  package_result ->
  string ->
  Riot_store.Manifest.export_entry option
