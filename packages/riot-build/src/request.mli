open Std

type scope =
  | Runtime
  | Dev

type t

val make:
  workspace:Riot_model.Workspace.t ->
  packages:Riot_model.Package_name.t list ->
  targets:Riot_model.Target.request ->
  scope:scope ->
  profile:Riot_model.Profile.t ->
  ?requested_parallelism:int ->
  unit ->
  t

module Internal: sig
  val workspace: t -> Riot_model.Workspace.t

  val packages: t -> Riot_model.Package_name.t list

  val targets: t -> Riot_model.Target.request

  val scope: t -> scope

  val profile: t -> Riot_model.Profile.t

  val requested_parallelism: t -> int option
end
