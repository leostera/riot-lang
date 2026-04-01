open Std

val read:
  workspace_root:Path.t ->
  (Tusk_model.Lockfile.t option, string) result

val write:
  workspace_root:Path.t ->
  Tusk_model.Lockfile.t ->
  (unit, string) result
