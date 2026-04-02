open Std

module Error = Error

type event_sink = Riot_model.Event.kind -> unit
val resolve_packages:
  ?emit:event_sink ->
  registry:Pkgs_ml.Registry.t ->
  workspace_root:Path.t ->
  packages:Riot_model.Package.t list ->
  lockfile:Riot_model.Lockfile.t ->
  unit ->
  (Riot_model.Package.resolved list, Error.t) result
