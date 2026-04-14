open Std

type scope =
  | Runtime
  | Dev

type t

val make:
  workspace:Prepared_workspace.t ->
  packages:string list ->
  targets:Riot_model.Target.request ->
  scope:scope ->
  profile:Riot_model.Profile.t ->
  unit ->
  t

module Internal: sig
  val workspace: t -> Prepared_workspace.t

  val packages: t -> string list

  val targets: t -> Riot_model.Target.request

  val scope: t -> scope

  val profile: t -> Riot_model.Profile.t
end
