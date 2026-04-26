open Std

type error =
  | ManifestLoadFailed of {
      manifest_path: Path.t;
      error: Riot_model.Workspace_manager.manifest_load_error;
    }
  | DependencySectionMustBeTable of {
      manifest_path: Path.t;
      section: string;
    }
  | ManifestMustBeTable of {
      manifest_path: Path.t;
    }
val error_message: error -> string

val dependency_hash:
  workspace_manager:Riot_model.Workspace_manager.t ->
  workspace_root:Path.t ->
  manifest_paths:Path.t list ->
  (string, error) result

val needs_refresh:
  workspace_manager:Riot_model.Workspace_manager.t ->
  workspace_root:Path.t ->
  manifest_paths:Path.t list ->
  lockfile:Riot_model.Lockfile.t option ->
  (bool, error) result
