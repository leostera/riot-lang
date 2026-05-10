open Std

type ocaml_source = {
  build: Goal.build_package;
  source: Source_analysis.key;
  module_name: Riot_model.Module_name.t;
}

type c_object = {
  build: Goal.build_package;
  source: Path.t;
  output: Path.t;
}

type ocaml_generated = {
  build: Goal.build_package;
  action: Action_execution.t;
}

let ocaml_source = fun ~build ~package (task: Riot_planner.Module_graph.source_analysis_task) -> {
  build;
  source = Source_analysis.key_from_task ~package task;
  module_name = Riot_model.Module_name.from_path task.task_path;
}

let c_object_output = fun source ->
  Path.remove_extension source
  |> Path.add_extension ~ext:"o"
  |> Path.basename
  |> Path.v

let c_object = fun ~build ~source -> {
  build;
  source;
  output = c_object_output source;
}

let ocaml_generated = fun ~build action -> { build; action }

let is_interface_source = fun (task: Riot_planner.Module_graph.source_analysis_task) ->
  Path.extension task.task_path = Some ".mli"

let is_implementation_source = fun (task: Riot_planner.Module_graph.source_analysis_task) ->
  Path.extension task.task_path = Some ".ml"
