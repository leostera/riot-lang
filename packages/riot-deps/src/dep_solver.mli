open Std

module Error = Error

type mode =
  | Refresh
  | Unlock
type event_sink = Riot_model.Event.deps_event -> unit
type context = {
  emit: event_sink;
  mode: mode;
  registry: Pkgs_ml.Registry.t;
  existing_lock: Riot_model.Lockfile.t option;
  workspace: Riot_model.Workspace_manifest.t;
}

val lock_deps:
  ?emit:event_sink ->
  mode:mode ->
  registry:Pkgs_ml.Registry.t ->
  existing_lock:Riot_model.Lockfile.t option ->
  workspace:Riot_model.Workspace_manifest.t ->
  unit ->
  (Riot_model.Lockfile.t, Error.t) result
