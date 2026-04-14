open Std

type scope = Build_runtime.build_scope =
  | Runtime
  | Dev

type t

val make:
  workspace:Prepared_workspace.t ->
  package_names:string list ->
  targets:Riot_model.Target.Set.t ->
  scope:scope ->
  profile:Riot_model.Profile.t ->
  t

val workspace: t -> Prepared_workspace.t

val package_names: t -> string list

val targets: t -> Riot_model.Target.Set.t

val scope: t -> scope

val profile: t -> Riot_model.Profile.t
