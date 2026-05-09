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

let key_from_task = fun ~package (task: Riot_planner.Module_graph.source_analysis_task) -> {
  package;
  path = task.task_path;
  module_path = task.task_module_path;
}

let make = fun ~package ~task -> { key = key_from_task ~package task; task }
