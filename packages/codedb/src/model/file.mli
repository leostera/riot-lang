open Std

type t = {
  path : Path.t;
  sha256 : string;
  size : int option;
  modified_at : Datetime.t option;
}

val make :
  path:Path.t ->
  sha256:string ->
  ?size:int ->
  ?modified_at:Datetime.t ->
  unit ->
  t
val entity_uri : t -> Poneglyph.Uri.t
val to_facts :
  tx_id:UUID.t -> ?stated_at:Datetime.t -> t -> Poneglyph.Fact.t list
val to_json : t -> Data.Json.t
val from_json : Data.Json.t -> (t, string) result
