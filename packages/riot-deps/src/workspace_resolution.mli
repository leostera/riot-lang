open Std

type event_sink = Riot_model.Event.kind -> unit

val ensure_lock:
  ?emit:event_sink ->
  ?existing_lock:Riot_model.Lockfile.t option ->
  workspace_manager:Riot_model.Workspace_manager.t ->
  mode:Dep_solver.mode ->
  registry:Pkgs_ml.Registry.t ->
  workspace:Riot_model.Workspace_manifest.t ->
  unit ->
  (Riot_model.Lockfile.t * Riot_model.Package.resolved list, Error.t) result

val ensure_workspace:
  ?emit:event_sink ->
  ?overrides:Riot_model.Workspace.overrides ->
  workspace_manager:Riot_model.Workspace_manager.t ->
  mode:Dep_solver.mode ->
  registry:Pkgs_ml.Registry.t ->
  workspace:Riot_model.Workspace_manifest.t ->
  unit ->
  (Riot_model.Workspace.t, Error.t) result
