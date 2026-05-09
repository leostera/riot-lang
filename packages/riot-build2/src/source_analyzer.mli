open Std

type t

val create: unit -> t

val find: t -> Source_analysis.key -> Riot_planner.Module_graph.source_analysis option

val missing:
  t ->
  package:Riot_model.Package_name.t ->
  Riot_planner.Module_graph.source_analysis_task list ->
  Source_analysis.t list

val execute: t -> Source_analysis.t -> (unit, Error.t) result

val analyze_from_cache:
  t ->
  Riot_model.Package.t ->
  on_source_analyzed:(Riot_planner.Module_graph.source_analysis_progress -> unit) ->
  Riot_planner.Module_graph.source_analysis_task list ->
  (Riot_planner.Module_graph.source_analysis, Riot_planner.Planning_error.t) result list
