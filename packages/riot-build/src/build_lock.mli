open Std

type t

val retry_interval: Time.Duration.t

val path:
  workspace:Riot_model.Workspace.t ->
  profile:string ->
  target:Riot_model.Target.t ->
  Path.t

val release: t -> unit

val wait:
  workspace:Riot_model.Workspace.t ->
  profile:string ->
  target:Riot_model.Target.t ->
  (t, exn) result

val acquire:
  workspace:Riot_model.Workspace.t ->
  profile:string ->
  target:Riot_model.Target.t ->
  (unit -> ('a, 'b) result) ->
  ('a, 'b) result
