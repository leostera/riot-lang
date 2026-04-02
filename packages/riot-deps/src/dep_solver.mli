open Std

module Error = Error

type mode =
  | Refresh
  | Unlock
type event_sink = Riot_model.Event.kind -> unit
type context = {
  emit: event_sink;
  mode: mode;
  registry: Pkgs_ml.Registry.t;
  existing_lock: Riot_model.Lockfile.t option;
  workspace: Riot_model.Workspace.t;
}
val lock_deps:
  ?emit:event_sink ->
  mode:mode ->
  registry:Pkgs_ml.Registry.t ->
  existing_lock:Riot_model.Lockfile.t option ->
  workspace:Riot_model.Workspace.t ->
  unit ->
  (Riot_model.Lockfile.t, Error.t) result
