open Std

type payload

val create_cache: store:Riot_store.Store.t -> payload Graph_cache.t

val input_hash:
  package:Riot_model.Package_name.t ->
  Riot_planner.Module_graph.source_analysis ->
  Crypto.hash

val payload:
  package:Riot_model.Package_name.t ->
  Riot_planner.Module_graph.source_analysis ->
  payload
