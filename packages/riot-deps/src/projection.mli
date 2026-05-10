open Std

module Error = Error

type event_sink = Riot_model.Event.deps_event -> unit

val resolve_packages:
  ?emit:event_sink ->
  ?materialize_emit:event_sink ->
  registry:Pkgs_ml.Registry.t ->
  workspace_root:Path.t ->
  packages:Riot_model.Package_manifest.t list ->
  lockfile:Riot_model.Lockfile.t ->
  unit ->
  (Riot_model.Package.resolved list, Error.t) result
