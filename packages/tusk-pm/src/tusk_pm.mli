open Std

module Error = Error
module Dep_solver = Dep_solver

module Lockfile_store = Lockfile_store

module Lock_refresh = Lock_refresh

module Projection = Projection

module Materializer = Materializer
module Publisher = Publisher

type event_sink = Tusk_model.Event.kind -> unit
val ensure_lock:
  ?emit:event_sink ->
  mode:Dep_solver.mode ->
  registry:Pkgs_ml.Registry.t ->
  workspace_root:Path.t ->
  manifest_paths:Path.t list ->
  packages:Tusk_model.Package.t list ->
  unit ->
  ((Tusk_model.Lockfile.t * Tusk_model.Package.resolved list), Error.t) result

val ensure_workspace:
  ?emit:event_sink ->
  mode:Dep_solver.mode ->
  registry:Pkgs_ml.Registry.t ->
  workspace:Tusk_model.Workspace.t ->
  unit ->
  (Tusk_model.Workspace.t, Error.t) result
