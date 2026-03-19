open Std

val start_local :
  workspace:Tusk_model.Workspace.t ->
  config:Server_config.t ->
  (Pid.t, exn) result
