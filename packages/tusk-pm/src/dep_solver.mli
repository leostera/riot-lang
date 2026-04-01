open Std

type mode =
  | Refresh
  | Unlock

val lock_deps:
  mode:mode ->
  registry_name:string ->
  Tusk_model.Package.t list ->
  (Tusk_model.Lockfile.t, string) result
