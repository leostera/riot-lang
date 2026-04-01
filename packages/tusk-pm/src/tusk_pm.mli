open Std

module Dep_solver = Dep_solver

module Lockfile_store = Lockfile_store

module Lock_refresh = Lock_refresh

module Projection = Projection

type event_sink = Tusk_model.Event.kind -> unit

val ensure_lock:
  ?emit:event_sink ->
  mode:Dep_solver.mode ->
  registry:Pkgs_ml.Registry.t ->
  registry_cache:Pkgs_ml.Registry_cache.t ->
  registry_name:string ->
  workspace_root:Path.t ->
  manifest_paths:Path.t list ->
  packages:Tusk_model.Package.t list ->
  unit ->
  ((Tusk_model.Lockfile.t * Tusk_model.Package.resolved list), string) result
