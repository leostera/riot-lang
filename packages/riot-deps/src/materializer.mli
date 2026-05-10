open Std

module Error = Error

val ensure_registry_package:
  ?emit:(Riot_model.Event.deps_event -> unit) ->
  registry:Pkgs_ml.Registry.t ->
  pkg:Riot_model.Lockfile.package ->
  unit ->
  (Path.t, Error.t) result

val ensure_packages:
  ?emit:(Riot_model.Event.deps_event -> unit) ->
  registry:Pkgs_ml.Registry.t ->
  lockfile:Riot_model.Lockfile.t ->
  unit ->
  (unit, Error.t) result
