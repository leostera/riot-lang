open Std

type event_sink = Tusk_model.Event.kind -> unit

val resolve_packages:
  ?emit:event_sink ->
  packages:Tusk_model.Package.t list ->
  lockfile:Tusk_model.Lockfile.t ->
  unit ->
  (Tusk_model.Package.resolved list, string) result
