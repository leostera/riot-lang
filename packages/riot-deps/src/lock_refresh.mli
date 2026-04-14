open Std

val dependency_hash:
  workspace_manager:Riot_model.Workspace_manager.t ->
  workspace_root:Path.t ->
  manifest_paths:Path.t list ->
  (string, string) result

val needs_refresh:
  workspace_manager:Riot_model.Workspace_manager.t ->
  workspace_root:Path.t ->
  manifest_paths:Path.t list ->
  lockfile:Riot_model.Lockfile.t option ->
  (bool, string) result
