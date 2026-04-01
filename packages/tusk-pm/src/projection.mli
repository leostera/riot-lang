open Std

val resolve_packages:
  packages:Tusk_model.Package.t list ->
  lockfile:Tusk_model.Lockfile.t ->
  (Tusk_model.Package.resolved list, string) result
