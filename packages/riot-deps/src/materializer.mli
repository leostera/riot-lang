open Std

module Error = Error

val ensure_packages:
  ?emit:(Riot_model.Event.kind -> unit) ->
  registry:Pkgs_ml.Registry.t ->
  lockfile:Riot_model.Lockfile.t ->
  unit ->
  (unit, Error.t) result
