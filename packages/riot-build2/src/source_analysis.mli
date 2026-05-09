open Std

type key = {
  package: Riot_model.Package_name.t;
  path: Path.t;
  module_path: string list option;
}
type t = {
  key: key;
  task: Riot_planner.Module_graph.source_analysis_task;
}

val key_of_task:
  package:Riot_model.Package_name.t ->
  Riot_planner.Module_graph.source_analysis_task ->
  key

val make:
  package:Riot_model.Package_name.t ->
  task:Riot_planner.Module_graph.source_analysis_task ->
  t
