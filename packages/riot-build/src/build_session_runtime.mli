open Std

type error =
  | RegistryInitializationFailed of { registry_name: string; error: string }
  | WorkspacePreparationFailed of { error: Riot_model.Pm_error.t }
  | UnexpectedException of { error: string }
val error_message: error -> string

val start:
  ?registry:Pkgs_ml.Registry.t ->
  ?registry_name:string ->
  workspace:Riot_model.Workspace.t ->
  config:Build_session_config.t ->
  unit ->
  (Pid.t, error) result
