type t =
  | RecordOnce
  | ReplayOnly
  | RecordAll
  | NewEpisodes

val to_string : t -> string

val from_string : string -> t option
