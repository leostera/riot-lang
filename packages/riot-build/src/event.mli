open Std

(** Structured events emitted by [riot-build].

    Use these events to drive human output, JSON output, or higher-level UI
    integrations without scraping terminal text.
*)
type t =
  | Pm of Riot_model.Event.t
  | BuildingTarget of { target: string; host: bool }
  | CacheGc of Riot_store.Cache_gc.event
  | Phase of phase
  | Streaming of Client.streaming_event

and runtime_phase =
  | TargetsResolved of { target_count: int }
  | ToolchainsEnsured of { target_count: int }
  | ToolchainsValidated of { target_count: int }
  | ClientConnecting
  | ClientConnected
  | TargetBuildStarted of { target: string; host: bool }
  | TargetBuildFinished of { target: string; result_count: int; had_partial_failure: bool }
  | CacheGenerationRecordingStarted of { lane_count: int; new_entry_count: int }
  | CacheGenerationRecorded of { lane_count: int; new_entry_count: int }
  | ReturningResults of { result_count: int; had_partial_failure: bool }

and cli_phase =
  | JsonTerminalEventEncodingStarted of { event: string; result_count: int option }
  | JsonTerminalEventEncoded of { event: string; result_count: int option }

and phase =
  | RuntimePhase of runtime_phase
  | CliPhase of cli_phase

(** Convert an event into a JSON payload when it has a machine-readable form. *)
val to_json: t -> Data.Json.t option
