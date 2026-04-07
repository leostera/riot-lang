open Std

(** Structured events emitted by [riot-build].

    Use these events to drive human output, JSON output, or higher-level UI
    integrations without scraping terminal text.
*)

type t =
  | (** Package-manager event forwarded from dependency preparation or external
         source resolution. *)
    Pm of Riot_model.Event.t
  | (** Build is starting for a target package or binary.

         [host = true] means the target is being built for the host toolchain
         rather than a non-host cross target.
     *)
    BuildingTarget of {
      target: string;
      host: bool;
    }
  | (** Cache garbage-collection event emitted by [riot-store]. *)
    CacheGc of Riot_store.Cache_gc.event
  | (** Streaming event forwarded from the local build runtime client. *)
    Streaming of Client.streaming_event

(** Convert an event into a JSON payload when it has a machine-readable form. *)
val to_json: t -> Data.Json.t option
