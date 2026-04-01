open Std

module Client = Client

module Internal_server = Internal_server

module Protocol = Protocol

module Server_config = Server_config

type error = Internal_server.error

val error_message : error -> string

val start_local:
  ?emit:(Tusk_model.Event.kind -> unit) ->
  ?registry:Pkgs_ml.Registry.t ->
  ?registry_name:string ->
  workspace:Tusk_model.Workspace.t ->
  ?load_errors:Tusk_model.Workspace_manager.load_error list ->
  config:Server_config.t ->
  unit ->
  (Pid.t, error) result
