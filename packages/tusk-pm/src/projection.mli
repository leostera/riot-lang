open Std

type event_sink = Tusk_model.Event.kind -> unit

val resolve_packages:
  ?emit:event_sink ->
  registry:Pkgs_ml.Registry.t ->
  workspace_root:Path.t ->
  packages:Tusk_model.Package.t list ->
  lockfile:Tusk_model.Lockfile.t ->
  unit ->
  (Tusk_model.Package.resolved list, string) result
