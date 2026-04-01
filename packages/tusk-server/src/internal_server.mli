open Std

val start_local:
  ?registry:Pkgs_ml.Registry.t ->
  ?registry_cache:Pkgs_ml.Registry_cache.t ->
  ?registry_name:string ->
  workspace:Tusk_model.Workspace.t ->
  ?load_errors:Tusk_model.Workspace_manager.load_error list ->
  config:Server_config.t ->
  unit ->
  (Pid.t, exn) result
