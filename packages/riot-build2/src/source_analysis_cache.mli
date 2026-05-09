open Std

type payload

val create_cache: store:Riot_store.Store.t -> payload Graph_cache.t

val input_hash:
  package:Riot_model.Package_name.t ->
  Source_analysis.t ->
  (Crypto.hash, Error.t) result

val payload:
  package:Riot_model.Package_name.t ->
  Riot_planner.Module_graph.source_analysis ->
  payload
