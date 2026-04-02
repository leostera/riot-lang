open Std

type t =
  | Pm of Riot_model.Event.t
  | BuildingTarget of { target: string; host: bool }
  | Streaming of Client.streaming_event
val to_json: t -> Data.Json.t option
