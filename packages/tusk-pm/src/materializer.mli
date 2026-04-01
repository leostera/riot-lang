open Std

val ensure_packages:
  ?emit:(Tusk_model.Event.kind -> unit) ->
  registry:Pkgs_ml.Registry.t ->
  lockfile:Tusk_model.Lockfile.t ->
  unit ->
  (unit, string) result
