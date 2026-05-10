open Std

type payload

val create_cache: store:Riot_store.Store.t -> payload Graph_cache.t

val input_hash:
  package:Riot_model.Package_name.t ->
  Riot_planner.Module_graph.source_analysis ->
  Crypto.hash

val input_hash_for_task:
  package:Riot_model.Package_name.t ->
  task:Riot_planner.Module_graph.source_analysis_task ->
  source_hash:Crypto.hash ->
  Crypto.hash

val payload:
  package:Riot_model.Package_name.t ->
  Riot_planner.Module_graph.source_analysis ->
  payload option

val analysis:
  task:Riot_planner.Module_graph.source_analysis_task ->
  payload ->
  (Riot_planner.Module_graph.source_analysis, Riot_planner.Planning_error.t) result

val source_hash: payload -> Crypto.hash

val summary_hash: Riot_planner.Dep_analyzer.source_summary -> (Crypto.hash, Error.t) result

val summary_hash_of_analysis:
  Riot_planner.Module_graph.source_analysis ->
  (Crypto.hash, Error.t) result
