open Std

type scope = Request.scope =
  | Runtime
  | Dev

type t

val make:
  workspace:Prepared_workspace.t ->
  package_names:Riot_model.Package_name.t list ->
  targets:Riot_model.Target.Set.t ->
  scope:scope ->
  profile:Riot_model.Profile.t ->
  t

val workspace: t -> Prepared_workspace.t

val package_names: t -> Riot_model.Package_name.t list

val targets: t -> Riot_model.Target.Set.t

val scope: t -> scope

val profile: t -> Riot_model.Profile.t
