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
  workspace_root: Path.t;
  workspace_packages: Tusk_model.Package.t list;
}
val lock_deps:
  ?emit:event_sink ->
  mode:mode ->
  registry:Pkgs_ml.Registry.t ->
  existing_lock:Tusk_model.Lockfile.t option ->
  workspace_root:Path.t ->
  Tusk_model.Package.t list ->
  (Tusk_model.Lockfile.t, Error.t) result
