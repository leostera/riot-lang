open Std

type build_scope =
  | Runtime
  | Dev
type target_request = Target_selector.t =
  | Host
  | All
  | Pattern of string
type build_request = {
  workspace: Riot_model.Workspace.t;
  packages: string list;
  targets: target_request;
  scope: build_scope;
  profile: string;
}
type build_phase = Event.phase =
  | RuntimePhase of Event.runtime_phase
  | CliPhase of Event.cli_phase
type build_event = Event.t =
  | Pm of Riot_model.Event.t
  | BuildingTarget of { target: string; host: bool }
  | CacheGc of Riot_store.Cache_gc.event
  | Phase of build_phase
  | Streaming of Client.streaming_event
type build_error =
  | NoTargetsMatched of Target_selector.error
  | ToolchainInstallFailed of { target: string; error: string }
  | ToolchainInitializationFailed of { target: string; error: string }
  | ClientError of Client.error
val error_message: build_error -> string

val build:
  ?record_cache_generation:bool ->
  ?on_event:(build_event -> unit) ->
  ?workspace_manager:Riot_model.Workspace_manager.t ->
  build_request ->
  (Riot_executor.Package_builder.build_result list, build_error) result

val build_best_effort:
  ?record_cache_generation:bool ->
  ?on_event:(build_event -> unit) ->
  ?workspace_manager:Riot_model.Workspace_manager.t ->
  build_request ->
  (Riot_executor.Package_builder.build_result list, build_error) result

val build_prepared:
  ?record_cache_generation:bool ->
  ?on_event:(build_event -> unit) ->
  ?workspace_manager:Riot_model.Workspace_manager.t ->
  build_request ->
  (Riot_executor.Package_builder.build_result list, build_error) result
