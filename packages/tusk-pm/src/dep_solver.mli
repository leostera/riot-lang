open Std

type mode =
  | Refresh
  | Unlock
type event_sink = Tusk_model.Event.kind -> unit
val lock_deps:
  ?emit:event_sink ->
  mode:mode ->
  registry:Pkgs_ml.Registry.t ->
  existing_lock:Tusk_model.Lockfile.t option ->
  Tusk_model.Package.t list ->
  (Tusk_model.Lockfile.t, string) result
