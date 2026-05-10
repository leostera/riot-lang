open Std

type t

val create: Riot_model.Workspace.t -> t

val begin_execution: t -> unit

val workspace: t -> Riot_model.Workspace.t

val manifests: t -> Riot_model.Package_manifest.t list

val package_names: t -> Riot_model.Package_name.t list

val find_manifest: t -> Riot_model.Package_name.t -> Riot_model.Package_manifest.t option

val require_manifest:
  t ->
  Riot_model.Package_name.t ->
  (Riot_model.Package_manifest.t, Error.t) result

val dependency_names_for_scope:
  t ->
  scope:Riot_model.Package.dependency_scope ->
  Riot_model.Package_name.t ->
  (Riot_model.Package_name.t list, Error.t) result

val realize:
  t ->
  intent:Riot_model.Package.realization_intent ->
  Riot_model.Package_name.t ->
  (Riot_model.Package.t, Error.t) result
