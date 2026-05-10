type t =
  | RecordOnce
  | ReplayOnly
  | RecordAll
  | NewEpisodes

let to_string = fun value ->
  match value with
  | RecordOnce -> "record_once"
  | ReplayOnly -> "replay_only"
  | RecordAll -> "record_all"
  | NewEpisodes -> "new_episodes"

let from_string = fun value ->
  match value with
  | "record_once" -> Some RecordOnce
  | "replay_only" -> Some ReplayOnly
  | "record_all" -> Some RecordAll
  | "new_episodes" -> Some NewEpisodes
  | _ -> None
