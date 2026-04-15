open Std

type build_event =
  | Pm of Riot_model.Event.t
  | BuildingTarget of { target: Riot_model.Target.t; host: bool }
  | CacheGc of Riot_store.Cache_gc.event
  | Phase of Event.runtime_phase
  | Streaming of Build_session.streaming_event
type build_error =
  | ToolchainInstallFailed of { target: Riot_model.Target.t; error: string }
  | ToolchainInitializationFailed of { target: Riot_model.Target.t; error: string }
  | BuildSessionError of Build_session.error
val error_message: build_error -> string

val execute:
  ?allow_partial_failures:bool ->
  ?record_cache_generation:bool ->
  ?on_event:(build_event -> unit) ->
  Build_spec.t ->
  (Package_builder.build_result list, build_error) result
