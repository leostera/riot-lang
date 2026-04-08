open Std

(** Structured events emitted by [riot-build].

    Use these events to drive human output, JSON output, or higher-level UI
    integrations without scraping terminal text.
*)
type t =
  | Pm of Riot_model.Event.t
  | BuildingTarget of { target: string; host: bool }
  | CacheGc of Riot_store.Cache_gc.event
  | Streaming of Client.streaming_event

(** Convert an event into a JSON payload when it has a machine-readable form. *)
val to_json: t -> Data.Json.t option
