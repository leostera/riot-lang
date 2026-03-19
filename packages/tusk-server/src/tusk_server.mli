open Std

module Internal_server = Internal_server
module Protocol = Protocol
module Server_config = Server_config

val start_local :
  workspace:Tusk_model.Workspace.t ->
  config:Server_config.t ->
  (Pid.t, exn) result
