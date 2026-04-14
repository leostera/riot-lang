open Std

type t

val retry_interval: Time.Duration.t

val path:
  target_dir_root:Path.t ->
  profile:string ->
  target:Riot_model.Target.t ->
  Path.t

val release: t -> unit

val wait:
  target_dir_root:Path.t ->
  profile:string ->
  target:Riot_model.Target.t ->
  (t, exn) result

val acquire:
  target_dir_root:Path.t ->
  profile:string ->
  target:Riot_model.Target.t ->
  (unit -> ('a, 'b) result) ->
  ('a, 'b) result
