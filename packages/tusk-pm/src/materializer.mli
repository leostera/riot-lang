open Std

val ensure_packages:
  ?emit:(Tusk_model.Event.kind -> unit) ->
  registry:Pkgs_ml.Registry.t ->
  registry_cache:Pkgs_ml.Registry_cache.t ->
  lockfile:Tusk_model.Lockfile.t ->
  unit ->
  (unit, string) result
