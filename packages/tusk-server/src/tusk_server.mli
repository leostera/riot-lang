open Std

module Internal_server = Internal_server

module Protocol = Protocol

module Server_config = Server_config

val start_local:
  workspace:Tusk_model.Workspace.t ->
  ?load_errors:Tusk_model.Workspace_manager.load_error list ->
  config:Server_config.t ->
  unit ->
  (Pid.t, exn) result
