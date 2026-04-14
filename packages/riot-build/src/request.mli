open Std

type scope = Build_runtime.build_scope =
  | Runtime
  | Dev

type t

val make:
  packages:string list ->
  targets:Riot_model.Target.request ->
  scope:scope ->
  profile:Riot_model.Profile.t ->
  unit ->
  t

val packages: t -> string list

val targets: t -> Riot_model.Target.request

val scope: t -> scope

val profile: t -> Riot_model.Profile.t
