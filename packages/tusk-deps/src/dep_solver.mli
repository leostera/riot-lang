open Std

module Error = Error

type mode =
  | Refresh
  | Unlock
type event_sink = Tusk_model.Event.kind -> unit
type context = {
  emit: event_sink;
  mode: mode;
  registry: Pkgs_ml.Registry.t;
  existing_lock: Tusk_model.Lockfile.t option;
  workspace: Tusk_model.Workspace.t;
}
val lock_deps:
  ?emit:event_sink ->
  mode:mode ->
  registry:Pkgs_ml.Registry.t ->
  existing_lock:Tusk_model.Lockfile.t option ->
  workspace:Tusk_model.Workspace.t ->
  unit ->
  (Tusk_model.Lockfile.t, Error.t) result
