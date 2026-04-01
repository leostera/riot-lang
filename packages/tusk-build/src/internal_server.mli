open Std

type error =
  | RegistryInitializationFailed of {
      registry_name: string;
      error: string
    }
  | WorkspacePreparationFailed of {
      error: Tusk_model.Pm_error.t
    }
  | UnexpectedException of {
      error: string
    }

val error_message : error -> string

val start_local:
  ?emit:(Tusk_model.Event.kind -> unit) ->
  ?registry:Pkgs_ml.Registry.t ->
  ?registry_name:string ->
  workspace:Tusk_model.Workspace.t ->
  config:Server_config.t ->
  unit ->
  (Pid.t, error) result
