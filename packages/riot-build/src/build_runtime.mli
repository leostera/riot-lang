open Std

type build_scope =
  | Runtime
  | Dev
type build_request = {
  workspace: Riot_model.Workspace.t;
  packages: string list;
  targets: Riot_model.Target.request;
  scope: build_scope;
  profile: string;
}
type build_phase = Event.phase =
  | RuntimePhase of Event.runtime_phase
  | CliPhase of Event.cli_phase
type build_event = Event.t =
  | Pm of Riot_model.Event.t
  | BuildingTarget of { target: Riot_model.Target.t; host: bool }
  | CacheGc of Riot_store.Cache_gc.event
  | Phase of build_phase
  | Streaming of Client.streaming_event
type build_error =
  | NoTargetsMatched of Riot_model.Target.resolve_error
  | ToolchainInstallFailed of { target: Riot_model.Target.t; error: string }
  | ToolchainInitializationFailed of { target: Riot_model.Target.t; error: string }
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
