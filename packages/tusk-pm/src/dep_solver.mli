open Std

type mode =
  | Refresh
  | Unlock
val lock_deps:
  mode:mode ->
  registry:Pkgs_ml.Registry.t ->
  registry_cache:Pkgs_ml.Registry_cache.t ->
  registry_name:string ->
  existing_lock:Tusk_model.Lockfile.t option ->
  Tusk_model.Package.t list ->
  (Tusk_model.Lockfile.t, string) result
